{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Cmm.I386Instr where

import Cmm.LabelGenerator
import Cmm.Backend          (MachineInstr(..), MachineFunction(..), MachinePrg(..))
import Data.Int

data UnaryInstr = PUSH
                | POP
                | NEG
                | NOT
                | INC
                | DEC
                | IDIV
    deriving Show

data BinayInstr = MOV
                | ADD
                | SUB
                | SHL
                | SHR
                | SAL
                | SAR
                | AND
                | OR
                | XOR
                | TEST
                | CMP
                | LEA
                | IMUL
    deriving Show

data Cond = E | NE | L | LE | G | GE | Z
  deriving Show

data Scale = S2 | S4 | S8 -- possible scaling values for effective addressing
  deriving Show

scaleToInt :: Scale -> Int
scaleToInt S2 = 2
scaleToInt S4 = 4
scaleToInt S8 = 8



data EffectiveAddress = EffectiveAddress
   { base         :: Maybe Temp
   , indexScale   :: Maybe (Temp, Scale)
   , displacement :: Int
   } deriving Show

data Operand = Imm Int32
             | Reg Temp
             | Mem EffectiveAddress
    deriving Show

data X86Instr = Unary UnaryInstr Operand
              | Binary BinayInstr Operand Operand
              | LABEL Label
              | CDQ
              | JMP Label
              | J Cond Label
              | CALL Label
              | RET
              | NOP
    deriving Show

data X86Func = X86Func {
    x86functionName :: String
  , x86body         :: [X86Instr]
  } deriving Show

data X86Prog = X86Prog {
    x86functions :: [X86Func]
  } deriving Show

-- | bypassing non exisiting module system of haskell
data X86CodeGen = X86CodeGen


-- | Instance definition for X86 Backend

-- |                   p       f        i 
instance MachinePrg X86Prog X86Func X86Instr  where

--  machinePrgFunctions :: p -> [f] 
    machinePrgFunctions p = x86functions p

--  replaceFunctions :: p -> [f] -> p 
    replaceFunctions p fs = undefined


-- |                        f       i
instance MachineFunction X86Func X86Instr where

--  machineFunctionName  :: f -> String
    machineFunctionName f = x86functionName f

--  machineFunctionBody  :: f -> [i]
    machineFunctionBody f = x86body f

--  machineFunctionRename :: f -> (Temp -> Temp) -> f
    machineFunctionRename f replaceTemp = undefined

--  machineFunctionSpill :: MonadNameGen m => f -> [Temp] -> m f
    machineFunctionSpill f ts = undefined
    

-- |                     i
instance MachineInstr X86Instr where

--  use  :: i -> [Temp] 
    use _                = undefined

--  def  :: i -> [Temp]
    def _                = undefined

--  isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
    isMoveBetweenTemps _ = undefined

--  isAssignmentToTemp :: i -> Maybe Temp
    isAssignmentToTemp _ = undefined

--  jumps :: i -> [Label]
    jumps _              = undefined

--  isFallThrough :: i -> Bool
    isFallThrough _      = undefined

--  isLabel :: i -> Maybe Label
    isLabel _            = undefined

--  renameInstr :: i -> (Temp -> Temp) -> i
    renameInstr _        = undefined


