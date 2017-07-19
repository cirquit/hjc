{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cmm.X86.Backend where

import Prelude                              hiding  (or, and)

import qualified Data.Map                   as Map
import           Data.Int
import           Data.Bool                  as B
import           Data.List                          (genericLength)
import           Data.List.Extra                    (concatUnzip)

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens                       hiding ((#))
import           Control.Monad                      (when, zipWithM_)

import           Cmm.CAST                   as CA
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.Backend
import           Cmm.I386Instr              as XI
import           Cmm.X86.Core

-- | Main function
--
generatex86 :: Cmm -> IO X86Prog
generatex86 cmm = runNameGenT $ codeGen X86CodeGen cmm

cmm2x86prog :: (MonadNameGen m, MonadIO m) => Cmm -> X86 m X86Prog
cmm2x86prog cmm = X86Prog <$> mapM cmmMethod2x86 cmm

cmmMethod2x86 :: (MonadNameGen m, MonadIO m) => CmmMethod -> X86 m X86Func 
cmmMethod2x86 method = do
    (functionBody, comments) <- concatUnzip <$> mapM cmmStm2x86 (cmmBody method)
    let fullCore     = functionStart         ++ functionBody ++ functionEnd
        fullComments = functionStartComments ++ comments     ++ functionEndComments
    return $ X86Func { x86functionName = cmmMethodName method
                     , x86body         = fullCore
                     , x86comments     = fullComments
                     }

  where

    sizeDir :: Maybe SizeDirective
    sizeDir = Nothing              -- no need for this for without pointer arithmetic in fstart/fend

    functionStart :: [X86Instr]
    functionStart = [ Unary  PUSH (sizeDir, ebp)               -- staring new callframe
                    , Binary MOV  (sizeDir, ebp) (sizeDir,esp) -- moving end of previous callframe to current start 
                    ]

    functionStartComments:: [X86Comment]
    functionStartComments = [comment "function prolog start"
                            ,comment "---------------   end"]

    functionEnd :: [X86Instr]
    functionEnd = [ Binary MOV (sizeDir, eax) (sizeDir, returnTemp) -- save return value in designated register eax
                  , Binary MOV (sizeDir, esp) (sizeDir, ebp)        -- set end of frame to start of frame
                  , Unary  POP (sizeDir, ebp)                       -- get the previous start of frame (in ebp)
                  , RET ]                                           -- jump to whatever adress ebp points to

    functionEndComments :: [X86Comment]
    functionEndComments = [comment "function epilog start"]
           ++ replicate 2 (comment "---------------------")
                      ++  [comment "----------------- end"]

    returnTemp :: Operand
    returnTemp = Reg $ cmmReturn method

cmmStm2x86 :: (MonadNameGen m, MonadIO m) => CmmStm -> X86 m ([X86Instr], [X86Comment])
cmmStm2x86 (MOVE ce1 ce2) = returnWithSideEffects $ do
    op1 <- cmmExp2x86 ce1
    op2 <- cmmExp2x86 ce2
    mov op1 op2

cmmStm2x86 (CJUMP relOp ce1 ce2 l1 l2) = returnWithSideEffects $ do
    op1 <- cmmExp2x86 ce1
    op2 <- cmmExp2x86 ce2
    cmp op1 op2 # "comparing for " ++ show relOp
    case relOp of
        EQ_C -> jumpIfElse E  l1 l2 # "jumps for (=)"
        NE_C -> jumpIfElse NE l1 l2 # "jumps for (!=)"
        LT_C -> jumpIfElse L  l1 l2 # "jumps for (<)"
        GT_C -> jumpIfElse G  l1 l2 # "jumps for (>)"
        LE_C -> jumpIfElse LE l1 l2 # "jumps for (<=)"
        GE_C -> jumpIfElse GE l1 l2 # "jumps for (>=)"
        _    -> error $ "X86Backend.cmmStm2x86 - relOp " ++ show relOp ++ " is not implemented yet"

cmmStm2x86 (JUMP (NAME l) ls) = returnWithSideEffects $ jump  l # "stm - jump "
cmmStm2x86 (CA.LABEL l)       = returnWithSideEffects $ label l # "stm - label"
cmmStm2x86 stm@_              = error $ "X86Backend.cmmStm2x86 - stm " ++ show stm ++ " is not implemented yet"


cmmExp2x86 :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
cmmExp2x86 e@(CA.CALL (NAME l) args) = callx86  e
cmmExp2x86 e@(PARAM   n)             = paramx86 e
cmmExp2x86 e@(BINOP binOp e1 e2)     = binopx86 e
cmmExp2x86 e@(MEM   exp)             = memx86   e
cmmExp2x86 (CONST i32)               = return $ Imm i32
cmmExp2x86 (TEMP    t)               = return $ Reg t
cmmExp2x86 (NAME    l)  = error $ "X86Backend.cmmExp2x86 - NAME with " ++ show l ++ " is shall not be parsed by this function, it's matched only for certain cases"
cmmExp2x86 (ESEQ _ _)   = error $ "X86Backend.cmmExp2x86 - ESEQ is not defined for the backend, use canonized code (canonizeCmm flag)"
cmmExp2x86 exp          = error $ "X86Backend.cmmExp2x86 - " ++ show exp ++ " is not defined yet"

-- | Function convention
--
--  Push arguments in reverse order on stack
--  call function
--  remove arguments from stack
--
callx86 :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
callx86 (CA.CALL (NAME l) args) = do
    ops <- mapM cmmExp2x86 args
    s   <- getScaleAsInt32
    mapM_ (\(o, i) -> push o  # "pushing " ++ show i ++ " argument") (argList ops)
    call l                    # "result of function call " ++ l ++ " is in eax by convention"
    add  esp (argStackSize s) # "remove arguments from stack"
    return eax

  where

    argList :: [Operand] -> [(Operand, Int)]
    argList ops = zip (reverse ops) [length ops, (length ops - 1)..]

    argStackSize :: Int32 -> Operand
    argStackSize s = Imm (s * genericLength args)
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
-- | function arguments lie above the ebp, 
--   we scale and use increment by 2 to point to the 0'th argument
--
paramx86 :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
paramx86 (PARAM n) = do
    s <- getScaleAsInt32
    let n32 = fromInteger n :: Int32
    return . Mem
           $ EffectiveAddress 
           { base         = Just ebpT
           , indexScale   = Nothing
           , displacement = (2*s + n32*s)
           }

-- | using ecx as first argument holder, and eax as second argument
--
binopx86 :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
binopx86 (BINOP binOp e1 e2) = do
    nop # "binop " ++ litShow binOp
    op1 <- cmmExp2x86 e1
    push op1      # "saving first "  ++ show binOp ++ " result"

    op2 <- cmmExp2x86 e2
    pop ecx       # "getting first " ++ show binOp ++ " result"
    mov eax op2   # "saving second " ++ show binOp ++ " result"

    case binOp of
        PLUS_C  -> add  ecx eax
        MINUS_C -> sub  ecx eax
        MUL_C   -> imul ecx eax
        AND_C   -> and  ecx eax 
        OR_C    -> or   ecx eax 
        XOR_C   -> xor  ecx eax 
        DIV_C   -> do 
            xor ecx eax  # "switch values, idiv saves the result in eax"
            xor eax ecx
            xor ecx eax
            cdq
            idiv ecx
            mov ecx eax
        _       -> error $ "X86Backend.cmmExp2x86 - binOp " ++ show binOp ++ " is not implemented yet"

    mov eax ecx
    return eax

-- | This dereferences the pointer
--   It should be a valid temporary - we currently don't allow pointer -> pointer (may be a problem with arrays)
memx86 :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m Operand
memx86 (MEM exp) = do
    op <- cmmExp2x86 exp
    case op of
        (Reg t) -> return . Mem
                          $ EffectiveAddress {
                              base         = Just t
                            , indexScale   = Nothing
                            , displacement = 0
                            }
        _       -> error $ "X86Backend.cmmExp2x86 - MEM had to dereference an invalid operand: " ++ show op


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