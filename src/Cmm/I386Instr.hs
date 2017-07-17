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

instance Show UnaryInstr where
  show PUSH = "push"
  show POP  = "pop"
  show NEG  = "neg"
  show NOT  = "not"
  show INC  = "inc"
  show DEC  = "dec"
  show IDIV = "idiv"

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

instance Show BinayInstr where
    show MOV  = "mov"
    show ADD  = "add"
    show SUB  = "sub"
    show SHL  = "shl"
    show SAL  = "sal"
    show SAR  = "sar"
    show AND  = "and"
    show OR   = "or"
    show XOR  = "xor"
    show TEST = "test"
    show CMP  = "cmp"
    show LEA  = "lea"
    show IMUL = "imul"

data Cond = E | NE | L | LE | G | GE | Z
  deriving Show

data Scale = S2 | S4 | S8 -- possible scaling values for effective addressing
  deriving Show

scaleToInt :: Scale -> Int32
scaleToInt S2 = 2
scaleToInt S4 = 4
scaleToInt S8 = 8

data EffectiveAddress = EffectiveAddress
   { base         :: Maybe Temp
   , indexScale   :: Maybe (Temp, Scale)
   , displacement :: Int32
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

data X86Func = X86Func {
    x86functionName :: String
  , x86body         :: [X86Instr]
  } 

data X86Prog = X86Prog {
    x86functions :: [X86Func]
  }

instance Show X86Prog where
    show p = concat ["\t.intel_syntax noprefix\n\t", ".global Lmain\n"]
          ++ concatMap (\x -> show x ++ "\n\t") (x86functions p)

instance Show X86Func where
    show f = x86functionName f ++ ":\n\t" ++
             concatMap (\x -> show x ++ "\n\t") (x86body f)

instance Show X86Instr where
    show (Unary u o)      = show u ++ ' ' : show o
    show (Binary b o1 o2) = show b ++ ' ' : show o1 ++ ", " ++ show o2
    show (LABEL l)        = l ++ ":"
    show RET              = "ret"
    show CDQ              = "cdq"
    show (JMP l)          = "jmp " ++ l
    show (J cond l)       = "j " ++ show cond ++ " " ++ l
    show (CALL l)         = "call " ++ l
    show NOP              = ""

instance Show Operand where
    show (Reg t)   = show t
    show (Imm i32) = show i32
    show (Mem ea)  = show ea

instance Show EffectiveAddress where
  
    show ea = 
        let c = show (displacement ea) in
        case (base ea, indexScale ea) of
            (Just bt, Just (t, s)) -> "[" ++ show bt ++ " + " ++ show t ++ "*" ++ show s ++ " + " ++ c ++ "]"
            (_,       Just (t, s)) -> "[" ++                     show t ++ "*" ++ show s ++ " + " ++ c ++ "]"
            (Just bt, _)           -> "[" ++ show bt                                     ++ " + " ++ c ++ "]"
            (_, _)                 -> "[" ++                                                         c ++ "]"

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


