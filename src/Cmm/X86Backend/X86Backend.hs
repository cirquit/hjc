{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Cmm.X86Backend.X86Backend where

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
      , _sideEffects          :: [X86Instr]
      , _sideEffectComments   :: [X86Comment]
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
              , _sideEffectComments = []
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
    (functionBody, comments) <- concatUnzip <$> mapM cmmStm2x86Instr (cmmBody method)
    let functionCore = (functionStart s) ++ functionBody ++ (functionEnd s)
        fullComments = functionStartComments ++ comments ++ functionEndComments
    return $ X86Func (cmmMethodName method) functionCore fullComments

  where

    functionStart :: Maybe SizeDirective -> [X86Instr]
    functionStart s = [ Unary  PUSH (s, ebp)         -- staring new callframe
                      , Binary MOV  (s, ebp) (s,esp) -- moving end of previous callframe to current start 
                      ]

    functionStartComments:: [X86Comment]
    functionStartComments = [comment "function prolog start"
                            ,comment "---------------   end"]

    functionEnd :: Maybe SizeDirective -> [X86Instr]
    functionEnd s = [ Binary MOV (s, eax) (s, returnTemp) -- save return value in designated register eax
                    , Binary MOV (s, esp) (s, ebp)        -- set end of frame to start of frame
                    , Unary  POP (s, ebp)                 -- get the previous start of frame (in ebp)
                    , RET ]                               -- jump to whatever adress ebp points to

    functionEndComments :: [X86Comment]
    functionEndComments = [comment "function epilog start"]
           ++ replicate 2 (comment "---------------------")
                      ++  [comment "----------------- end"]

    returnTemp :: Operand
    returnTemp = Reg $ cmmReturn method

cmmStm2x86Instr :: (MonadNameGen m, MonadIO m) => CmmStm -> X86 m ([X86Instr], [X86Comment])
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

cmmStm2x86Instr (JUMP (NAME l) ls) = return $ ([JMP l], [comment "stm - jump "])
cmmStm2x86Instr (CA.LABEL l)       = return $ ([XI.LABEL l], [comment "stm - label"] )
cmmStm2x86Instr stm@_              = error $ "X86Backend.cmmStm2x86Instr - stm " ++ show stm ++ " is not implemented yet"


cmmExp2x86Instr :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
cmmExp2x86Instr (CA.CALL (NAME l) args) = do
    ops <- mapM cmmExp2x86Instr args
    s   <- getScaleAsInt32
    let argByteLength = Imm (s * genericLength args)
    mapM_ (\(o, i) -> push_c o (comment $ "pushing " ++ show i ++ " arg of " ++ l)) $ zip (reverse ops) ([length ops, (length ops - 1)..])             -- push arguments in reverse order (by convention)
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
    push_c op1 (comment $ "saving first " ++ show binOp ++ " result")

    op2 <- cmmExp2x86Instr e2
    pop_c ecx     (comment $ "getting first " ++ show binOp ++ " result")
    mov_c eax op2 (comment $ "saving second " ++ show binOp ++ " result")

    case binOp of
        PLUS_C  -> add  ecx eax
        MINUS_C -> sub  ecx eax
        MUL_C   -> imul ecx eax
        AND_C   -> and  ecx eax 
        OR_C    -> or   ecx eax 
        XOR_C   -> xor  ecx eax 
        DIV_C   -> do 
            xor_c ecx eax (comment "switch values, idiv saves the result in eax")
            xor eax ecx
            xor ecx eax
            cdq
            idiv ecx
            mov ecx eax
        _       -> error $ "X86Backend.cmmExp2x86Instr - binOp " ++ show binOp ++ " is not implemented yet"

    mov eax ecx
    return eax


cmmExp2x86Instr (MEM   exp) = do
    op <- cmmExp2x86Instr exp
    case op of
        (Reg t) -> return . Mem
                          $ EffectiveAddress {
                              base = Just t
                            , indexScale = Nothing
                            , displacement = 0
                            }
        _       -> error $ "X86Backend.cmmExp2x86Instr - MEM had to dereference an invalid operand: " ++ show op
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

lea_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
lea_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary LEA dop1 dop2) c

xor_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
xor_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary XOR dop1 dop2) c

and_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
and_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary AND dop1 dop2) c

or_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
or_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary OR dop1 dop2) c

add_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
add_c op1 op2 c =  do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary ADD dop1 dop2) c

sub_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
sub_c op1 op2 c = do
    dop1 <- getDefaultOperand op1
    dop2 <- getDefaultOperand op2
    addSideEffects (Binary SUB dop1 dop2) c

imul_c :: (MonadNameGen m, MonadIO m) => Operand -> Operand -> X86Comment ->X86 m ()
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

-- with comments
cdq_c :: (MonadNameGen m, MonadIO m) => X86Comment -> X86 m ()
cdq_c c = addSideEffects CDQ c 

jumpIfElse_c :: (MonadNameGen m, MonadIO m) => Cond -> Label -> Label -> X86Comment -> X86 m ()
jumpIfElse_c cnd ltrue lfalse c = jumpCnd_c cnd ltrue c >> jump lfalse

jumpCnd_c :: (MonadNameGen m, MonadIO m) => Cond -> Label -> X86Comment -> X86 m ()
jumpCnd_c cnd l c = addSideEffects (J cnd l) c

jump_c :: (MonadNameGen m, MonadIO m) => Label -> X86Comment -> X86 m ()
jump_c l c = addSideEffects (JMP l) c



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
