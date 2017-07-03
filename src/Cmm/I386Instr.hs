{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Cmm.I386Instr where

import Cmm.LabelGenerator
import Cmm.Backend (MachineInstr(..))
import Data.Int

data UnaryInstr = PUSH
                | POP
                | NEG
                | NOT
                | INC
                | DEC
                | IDIV

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

data Cond = E | NE | L | LE | G | GE | Z

data Scale = S2 | S4 | S8 -- possible scaling values for effective addressing

data EffectiveAddress = EffectiveAddress
   { base         :: Maybe Temp
   , indexScale   :: Maybe (Temp, Scale)
   , displacement :: Int
   }

data Operand = Imm Int32
             | Reg Temp
             | Mem EffectiveAddress

data X86Instr = Unary UnaryInstr Operand
              | Binary BinayInstr Operand Operand
              | LABEL Label
              | CDQ
              | JMP Label
              | J Cond Label
              | CALL Label
              | RET
              | NOP

instance MachineInstr X86Instr where
  def _                = error "needed later for register allocation"
  use _                = error "needed later for register allocation"
  isMoveBetweenTemps _ = error "needed later for register allocation"
  isAssignmentToTemp _ = error "needed later for register allocation"
  jumps _              = error "needed later for register allocation"
  isFallThrough _      = error "needed later for register allocation"
  isLabel _            = error "needed later for register allocation"
  renameInstr _        = error "needed later for register allocation"
