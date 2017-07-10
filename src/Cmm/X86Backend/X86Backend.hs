{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Cmm.X86Backend.X86Backend where


import qualified Data.Map                   as Map
import           Data.Int
import           Data.Bool                  as B

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
    } deriving Show

-- |                 c         p       f       i        
instance CodeGen X86CodeGen X86Prog X86Func X86Instr where 

--  codeGen :: MonadNameGen m => c -> Cmm -> m p 
    codeGen _ cmm = fst <$> runStateT (cmm2x86prog cmm) state
        where 
            state = X86State {
                _header = unlines ["        .intel_syntax"
                                  ,"        .global Lmain"]
              , _scale = S4
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
cmmMethod2x86Function method = 
    X86Func <$> pure (cmmMethodName method)
            <*> (concat <$> mapM cmmStm2x86Instr (cmmBody method))

cmmStm2x86Instr :: (MonadNameGen m, MonadIO m) => CmmStm -> X86 m [X86Instr]
cmmStm2x86Instr stm = return []

cmmExp2x86Instr :: (MonadNameGen m, MonadIO m) => CmmExp -> X86 m ([X86Instr], Operand)
cmmExp2x86Instr (CONST   i32) = return $ ([], Imm i32)
cmmExp2x86Instr (TEMP      t) = return $ ([], Reg t)
cmmExp2x86Instr (PARAM     i) = do -- TEST if this use of indexScale is correct
    s <- view scale <$> get
    return ([], Mem $ EffectiveAddress {
            base         = Just ebpTemp
          , indexScale   = Nothing
          , displacement = 4 * scaleToInt s
          })
-- cmmExp2x86Instr (NAME l)      = 
-- cmmExp2x86Instr (MEM exp)     =  
-- cmmExp2x86Instr (CA.CALL (NAME l) )  = return ([XI.CALL l], )

cmmExp2x86Instr (ESEQ _ _) = error $ "X86Backend.cmmExp2x86Instr - ESEQ is not defined for the backend, use canonized code (canonizeCmm flag)"
cmmExp2x86Instr exp = error $ "X86Backend.cmmExp2x86Instr - " ++ show exp ++ " is not defined yet"



ebpTemp :: Temp
ebpTemp = mkNamedTemp "ebp"

header   :: Lens' X86State String
header = lens _header (\x y -> x { _header = y })

scale   :: Lens' X86State Scale
scale = lens _scale (\x y -> x { _scale = y })


