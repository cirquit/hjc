{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Cmm.X86Backend.X86Backend where

import Prelude                              hiding  (or, and)

import qualified Data.Map                   as Map
import           Data.Int
import           Data.Bool                  as B
import           Data.List                          (genericLength)

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           Cmm.CAST                   as CA
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.Backend
import           Cmm.I386Instr              as XI

type X86 m = StateT X86State m

instance MonadNameGen m => MonadNameGen (X86 m) where 

--  nextTemp' :: m Temp 
    nextTemp'   = lift nextTemp'

--  avoid' :: [Temp] -> m ()
    avoid'      = lift . avoid'

--  nextLabel' :: m Label
    nextLabel'  = lift nextLabel'


data X86State = X86State 
    { 
        _header :: String
      , _scale :: Scale
      , _sideEffects :: [X86Instr]
      , _defaultSizeDirective :: SizeDirective
    } deriving Show

-- |                 c         p       f       i        
instance CodeGen X86CodeGen X86Prog X86Func X86Instr where 

--  codeGen :: MonadNameGen m => c -> Cmm -> m p 
    codeGen _ cmm = fst <$> runStateT (cmm2x86prog cmm) state
        where 
            state = X86State {
                _header = []
              , _scale = S4
              , _sideEffects = []
              , _defaultSizeDirective = DWORD
            }

--  allRegisters :: c -> [Temp]
    allRegisters c = undefined

--  generalPurposeRegisters :: c -> [Temp]
    generalPurposeRegisters c = undefined  


generatex86 :: Cmm -> IO X86Prog
generatex86 cmm = runNameGenT $ codeGen X86CodeGen cmm

cmm2x86prog :: (MonadNameGen m, MonadIO m) => Cmm -> X86 m X86Prog
cmm2x86prog cmm = X86Prog <$> mapM cmmMethod2x86Function cmm

cmmMethod2x86Function :: (MonadNameGen m, MonadIO m) => CmmMethod -> X86 m X86Func 
cmmMethod2x86Function method = do
    let s = Nothing :: Maybe SizeDirective -- no need for this for without pointer arithmetic in fstart/fend
    functionBody <- concat <$> mapM cmmStm2x86Instr (cmmBody method)
    let functionCore = (functionStart s) ++ functionBody ++ (functionEnd s)
    return $ X86Func (cmmMethodName method) functionCore

  where

    functionStart :: Maybe SizeDirective -> [X86Instr]
    functionStart s = [ Unary  PUSH (s, ebp)         -- staring new callframe
                      , Binary MOV  (s, ebp) (s,esp) -- moving end of previous callframe to current start 
                      ]

    functionEnd :: Maybe SizeDirective -> [X86Instr]
    functionEnd s = [ Binary MOV (s, eax) (s, returnTemp) -- save return value in designated register eax
                    , Binary MOV (s, esp) (s, ebp)        -- set end of frame to start of frame
                    , Unary  POP (s, ebp)                 -- get the previous start of frame (in ebp)
                    , RET ]                               -- jump to whatever adress ebp points to

    returnTemp :: Operand
    returnTemp = Reg $ cmmReturn method

cmmStm2x86Instr :: (MonadNameGen m, MonadIO m) => CmmStm -> X86 m [X86Instr]
cmmStm2x86Instr (MOVE ce1 ce2) = 
    returnWithSideEffects $ do
        op1 <- cmmExp2x86Instr ce1
        op2 <- cmmExp2x86Instr ce2
        mov op1 op2

cmmStm2x86Instr (CJUMP relOp ce1 ce2 l1 l2) = do
    returnWithSideEffects $ do
        op1 <- cmmExp2x86Instr ce1
        op2 <- cmmExp2x86Instr ce2
        cmp op1 op2
        case relOp of
            EQ_C -> jumpIfElse E  l1 l2
            NE_C -> jumpIfElse NE l1 l2
            LT_C -> jumpIfElse L  l1 l2
            GT_C -> jumpIfElse G  l1 l2
            LE_C -> jumpIfElse LE l1 l2
            GE_C -> jumpIfElse GE l1 l2
            _    -> error $ "X86Backend.cmmStm2x86Instr - relOp " ++ show relOp ++ " is not implemented yet"

cmmStm2x86Instr (JUMP (NAME l) ls) = return $ [JMP l]
cmmStm2x86Instr (CA.LABEL l)       = return $ [XI.LABEL l] 
cmmStm2x86Instr stm@_              = error $ "X86Backend.cmmStm2x86Instr - stm " ++ show stm ++ " is not implemented yet"


cmmExp2x86Instr :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
cmmExp2x86Instr (CA.CALL (NAME l) args) = do
    ops <- mapM cmmExp2x86Instr args
    s   <- getScaleAsInt32
    let argByteLength = Imm (s * genericLength args)
    mapM_ push (reverse ops)             -- push arguments in reverse order (by convention)
    call l
    add esp argByteLength                -- remove arguments from stack
    return eax                           -- every function call has to fill the return value in eax (by convention)

cmmExp2x86Instr (PARAM   n) = do
    s <- getScaleAsInt32
    let n32 = fromInteger n :: Int32
    return . Mem
           $ EffectiveAddress 
           { base = Just ebpT
           , indexScale = Nothing
           , displacement = (2*s + n32*s)
           }
           --
           --              |-Stack Layout-|
           --          n    
           --    ------|      __________   (+) addr ^
           --    PARAM 2  -> |__arg 3___|           |
           --    PARAM 1  -> |__arg 2___| 
           --    PARAM 0  -> |__arg 1___| 
           --                |____eip___|         
           --                |__old ebp_|<- ebp
           --                                       |
           --                              (-) addr v
           --       
cmmExp2x86Instr (BINOP binOp e1 e2) = do
    op1 <- cmmExp2x86Instr e1
    push op1

    op2 <- cmmExp2x86Instr e2
    
    pop ecx
    mov eax op2



    case binOp of
        PLUS_C  -> add  ecx eax
        MINUS_C -> sub  ecx eax
        MUL_C   -> imul ecx eax
        AND_C   -> and  ecx eax 
        OR_C    -> or   ecx eax 
        XOR_C   -> xor  ecx eax 
        DIV_C   -> do   -- switch values, idiv saves the result in eax
            xor ecx eax
            xor eax ecx
            xor ecx eax
            cdq
            idiv ecx
            mov ecx eax
        _       -> error $ "X86Backend.cmmExp2x86Instr - binOp " ++ show binOp ++ " is not implemented yet"

    mov eax ecx
    return eax


cmmExp2x86Instr (MEM   exp) = cmmExp2x86Instr exp
cmmExp2x86Instr (CONST i32) = return $ Imm i32
cmmExp2x86Instr (TEMP    t) = return $ Reg t
cmmExp2x86Instr (NAME    l) = error $ "X86Backend.cmmExp2x86Instr - NAME with " ++ show l ++ " is shall not be parsed by this function, it's matched only for certain cases"
cmmExp2x86Instr (ESEQ _ _)  = error $ "X86Backend.cmmExp2x86Instr - ESEQ is not defined for the backend, use canonized code (canonizeCmm flag)"
cmmExp2x86Instr exp         = error $ "X86Backend.cmmExp2x86Instr - " ++ show exp ++ " is not defined yet"

-- base pointer (register)
ebp :: Operand
ebp = Reg ebpT

ebpT :: Temp
ebpT = mkNamedTemp "%ebp"

-- stack pointer (register)
esp :: Operand
esp = Reg espT

espT :: Temp
espT = mkNamedTemp "%esp"

-- return register
eax :: Operand
eax = Reg eaxT

eaxT :: Temp
eaxT = mkNamedTemp "%eax"

-- math register (used for mathematic expressions)
ecx :: Operand
ecx = Reg ecxT

ecxT :: Temp
ecxT = mkNamedTemp "%ecx"

-- | Binary ops
--
mov :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
mov op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary MOV dop1 dop2])

lea :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
lea op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary LEA dop1 dop2])

xor :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
xor op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary XOR dop1 dop2])

and :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
and op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary AND dop1 dop2])

or :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
or op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary OR dop1 dop2])

add :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
add op1 op2 =  do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary ADD dop1 dop2])

sub :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
sub op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary SUB dop1 dop2])

imul :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
imul op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary IMUL dop1 dop2])

cmp :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86 m ()
cmp op1 op2 = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    sideEffects %= (++ [Binary CMP dop1 dop2])


-- | Unary ops
--
idiv :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
idiv op = do
    dop <- getDefaultOperand op
    sideEffects %= (++ [Unary IDIV dop])

push :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
push op =  do
    dop <- getDefaultOperand op
    sideEffects %= (++ [Unary PUSH dop])

pop :: (MonadNameGen m, MonadIO m) => Operand -> X86 m ()
pop op =  do
    dop <- getDefaultOperand op
    sideEffects %= (++ [Unary POP dop])

call :: (MonadNameGen m, MonadIO m) => Label -> X86 m ()
call l = sideEffects %= (++ [XI.CALL l])

-- | Others
--
cdq :: (MonadNameGen m, MonadIO m) => X86 m ()
cdq = sideEffects %= (++ [CDQ])

jumpIfElse :: (MonadNameGen m, MonadIO m) => Cond -> Label -> Label -> X86 m ()
jumpIfElse cnd ltrue lfalse = jumpCnd cnd ltrue >> jump lfalse

jumpCnd :: (MonadNameGen m, MonadIO m) => Cond -> Label -> X86 m ()
jumpCnd cnd l = sideEffects %= (++ [J cnd l])

jump :: (MonadNameGen m, MonadIO m) => Label -> X86 m ()
jump l = sideEffects %= (++ [JMP l])

-- | size directives are only for registers
--
getDefaultOperand :: Monad m => Operand -> X86 m (Maybe SizeDirective, Operand)
getDefaultOperand op = do
    case op of
        (Imm _) -> return (Nothing, op)
        (Reg _) -> return (Nothing, op)
        _       -> Just . view defaultSizeDirective <$> get >>= \sd -> return (sd, op)


-- | Utils
--
getScaleAsInt32 :: (MonadNameGen m, MonadIO m) => X86 m Int32
getScaleAsInt32 = scaleToInt <$> view scale <$> get

returnWithSideEffects :: (MonadNameGen m, MonadIO m) => X86 m a -> X86 m [X86Instr]
returnWithSideEffects f = do
    oldSideEffects <- view sideEffects <$> get
    sideEffects .= []
    f
    newSideEffects <- view sideEffects <$> get
    sideEffects .= oldSideEffects
    return $ newSideEffects

header   :: Lens' X86State String
header = lens _header (\x y -> x { _header = y })

scale   :: Lens' X86State Scale
scale = lens _scale (\x y -> x { _scale = y })

sideEffects   :: Lens' X86State [X86Instr]
sideEffects = lens _sideEffects (\x y -> x { _sideEffects = y })

defaultSizeDirective   :: Lens' X86State SizeDirective
defaultSizeDirective = lens _defaultSizeDirective (\x y -> x { _defaultSizeDirective = y })
