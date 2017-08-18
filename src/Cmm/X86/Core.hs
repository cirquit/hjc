{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Cmm.X86.Core where

import Prelude                              hiding  (or, and)

import qualified Data.Map                   as Map
import           Data.Int
import           Data.Bool                  as B
import           Data.List                          (genericLength)
import           Data.List.Extra                    (concatUnzip)

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens

import           Cmm.CAST                   as CA
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.Backend
import           Cmm.I386Instr              as XI

data X86State = X86State 
    {   _header :: String
      , _scale                :: Scale          -- word := scale * 8 (see '')
      , _sideEffects          :: [X86Instr]     -- used to track sideeffects from 'cmmExp2x86' 
      , _sideEffectComments   :: [X86Comment]   --
      , _defaultSizeDirective :: SizeDirective  -- currently support only global size directives (use 'withSizeDirective' otherwise)
    } deriving Show

header   :: Lens' X86State String
header = lens _header (\x y -> x { _header = y })

scale   :: Lens' X86State Scale
scale = lens _scale (\x y -> x { _scale = y })

sideEffects   :: Lens' X86State [X86Instr]
sideEffects = lens _sideEffects (\x y -> x { _sideEffects = y })

sideEffectComments   :: Lens' X86State [X86Comment]
sideEffectComments = lens _sideEffectComments (\x y -> x { _sideEffectComments = y })

defaultSizeDirective   :: Lens' X86State SizeDirective
defaultSizeDirective = lens _defaultSizeDirective (\x y -> x { _defaultSizeDirective = y })

type X86 m = StateT X86State m

instance MonadNameGen m => MonadNameGen (X86 m) where 
--  nextTemp' :: m Temp 
    nextTemp'   = lift nextTemp'
--  avoid' :: [Temp] -> m ()
    avoid'      = lift . avoid'
--  nextLabel' :: m Label
    nextLabel'  = lift nextLabel'

-- | Utils
--
-- | size directives are only for registers
--
getDefaultOperand :: Monad m => Operand -> X86 m (Maybe SizeDirective, Operand)
getDefaultOperand op = do
    case op of
        (Imm _) -> return (Nothing, op)
        (Reg _) -> return (Nothing, op)
        _       -> Just . view defaultSizeDirective <$> get >>= \sd -> return (sd, op)

getScaleAsInt32 :: (MonadNameGen m, MonadIO m) => X86 m Int32
getScaleAsInt32 = scaleToInt <$> view scale <$> get

returnWithSideEffects :: (MonadNameGen m, MonadIO m) => X86 m a -> X86 m ([X86Instr], [X86Comment])
returnWithSideEffects f = do
    oldSideEffects <- view sideEffects <$> get
    oldSideEffectComments <- view sideEffectComments <$> get
    sideEffects .= []
    sideEffectComments .= []
    f
    newSideEffects <- view sideEffects <$> get
    newSideEffectComments <- view sideEffectComments <$> get
    sideEffects .= oldSideEffects
    sideEffectComments .= oldSideEffectComments
    return $ (newSideEffects, newSideEffectComments)

addSideEffects :: (MonadNameGen m, MonadIO m) => X86Instr -> X86Comment -> X86 m ()
addSideEffects i c = do
    sideEffects %= (++ [i])
    sideEffectComments %= (++ [c])

withSizeDirective s f = do
    oldSizeDirective <- view defaultSizeDirective <$> get
    defaultSizeDirective .= s
    f
    defaultSizeDirective .= oldSizeDirective


isNonRegister :: Operand -> Bool
isNonRegister (Reg _) = False
isNonRegister _       = True

isMemoryOperand :: Operand -> Bool
isMemoryOperand (Mem _) = True
isMemoryOperand _       = False


-- | Simple DSL for x86 asm
-- |
-- | Binary ops
--
-- without comments
mov :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
mov op1 op2 = mov_c op1 op2 emptyComment

lea :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
lea op1 op2 = lea_c op1 op2 emptyComment

xor :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
xor op1 op2 = xor_c op1 op2 emptyComment

and :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
and op1 op2 = and_c op1 op2 emptyComment

or :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
or op1 op2 = or_c op1 op2 emptyComment

add :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
add op1 op2 =  add_c op1 op2 emptyComment

sub :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
sub op1 op2 = sub_c op1 op2 emptyComment

imul :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
imul op1 op2 = imul_c op1 op2 emptyComment

cmp :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
cmp op1 op2 = cmp_c op1 op2 emptyComment

-- with comments
mov_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
mov_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary MOV dop1 dop2) c

lea_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
lea_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary LEA dop1 dop2) c

xor_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
xor_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary XOR dop1 dop2) c

and_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
and_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary AND dop1 dop2) c

or_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
or_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary OR dop1 dop2) c

add_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
add_c op1 op2 c =  do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary ADD dop1 dop2) c

sub_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
sub_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary SUB dop1 dop2) c

imul_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
imul_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary IMUL dop1 dop2) c

cmp_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment -> X86 m ()
cmp_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary CMP dop1 dop2) c

-- | Unary ops
--
-- without comments
idiv :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
idiv op = idiv_c op emptyComment

push :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
push op = push_c op emptyComment

pop :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
pop op = pop_c op emptyComment

call :: (MonadNameGen m, MonadIO m) => Label -> X86 m ()
call l = call_c l emptyComment

-- with comments
idiv_c :: (MonadNameGen m, MonadIO m) => Operand -> X86Comment -> X86 m ()
idiv_c op c = do
    dop <- getDefaultOperand op
    addSideEffects (Unary IDIV dop) c

push_c :: (MonadNameGen m, MonadIO m) => Operand -> X86Comment -> X86 m ()
push_c op c =  do
    dop <- getDefaultOperand op
    addSideEffects (Unary PUSH dop) c

pop_c :: (MonadNameGen m, MonadIO m) => Operand -> X86Comment -> X86 m ()
pop_c op c =  do
    dop <- getDefaultOperand op
    addSideEffects (Unary POP dop) c

call_c :: (MonadNameGen m, MonadIO m) => Label -> X86Comment -> X86 m ()
call_c l c = addSideEffects (XI.CALL l) c

-- | Others
--
-- without comments
cdq :: (MonadNameGen m, MonadIO m) => X86 m ()
cdq = cdq_c emptyComment

jumpIfElse :: (MonadNameGen m, MonadIO m) => Cond -> Label -> Label -> X86 m ()
jumpIfElse cnd ltrue lfalse = jumpIfElse_c cnd ltrue lfalse emptyComment

jumpCnd :: (MonadNameGen m, MonadIO m) => Cond -> Label -> X86 m ()
jumpCnd cnd l = jumpCnd_c cnd l emptyComment

jump :: (MonadNameGen m, MonadIO m) => Label -> X86 m ()
jump l = jump_c l emptyComment

label :: (MonadNameGen m, MonadIO m) => Label -> X86 m ()
label l = label_c l emptyComment

nop :: (MonadNameGen m, MonadIO m) => X86 m ()
nop = nop_c emptyComment

-- with comments
cdq_c :: (MonadNameGen m, MonadIO m) => X86Comment -> X86 m ()
cdq_c c = addSideEffects CDQ c 

jumpIfElse_c :: (MonadNameGen m, MonadIO m) => Cond -> Label -> Label -> X86Comment -> X86 m ()
jumpIfElse_c cnd ltrue lfalse c = jumpCnd_c cnd ltrue c >> jump lfalse

jumpCnd_c :: (MonadNameGen m, MonadIO m) => Cond -> Label -> X86Comment -> X86 m ()
jumpCnd_c cnd l c = addSideEffects (J cnd l) c

jump_c :: (MonadNameGen m, MonadIO m) => Label -> X86Comment -> X86 m ()
jump_c l c = addSideEffects (JMP l) c

label_c :: (MonadNameGen m, MonadIO m) => Label -> X86Comment -> X86 m ()
label_c l c = addSideEffects (XI.LABEL l) c

nop_c :: (MonadNameGen m, MonadIO m) => X86Comment -> X86 m ()
nop_c c = addSideEffects NOP c

-- | comment shorthand
--

infixr 1 #
(#) :: (MonadNameGen m, MonadIO m) => X86 m () -> String -> X86 m () 
(#) = replaceComment

replaceComment :: (MonadNameGen m, MonadIO m) => X86 m () -> String -> X86 m () 
replaceComment f c = do
    f
    sideEffectComments %= replaceLast (X86Comment c)

replaceLast :: a -> [a] -> [a]
replaceLast y []     = [y]
replaceLast y [x]    = [y] 
replaceLast y (x:xs) = x : replaceLast y xs


-- | label generator
nextTempO :: (MonadNameGen m) => X86 m Operand
nextTempO = Reg <$> nextTemp'
