module Cmm.X86.InstrCore where

import Cmm.LabelGenerator

import Data.Int
import Text.Printf

-- | bypassing non exisiting module system of haskell
data X86CodeGen = X86CodeGen

data SizeDirective =
      BYTE
    | WORD
    | DWORD
    | QWORD -- 64 bit won't be supported for the time being
  deriving (Eq, Ord)

instance Show SizeDirective where
    show BYTE  = "BYTE"
    show WORD  = "WORD"
    show DWORD = "DWORD PTR"
    show QWORD = "QWORD PTR"

data UnaryInstr =
      PUSH
    | POP
    | NEG
    | NOT
    | INC
    | DEC
    | IDIV
  deriving (Eq, Ord)

instance Show UnaryInstr where
  show PUSH = "push"
  show POP  = "pop"
  show NEG  = "neg"
  show NOT  = "not"
  show INC  = "inc"
  show DEC  = "dec"
  show IDIV = "idiv"

data BinayInstr =
      MOV
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
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

-- used with 'j' prefix
instance Show Cond where
    show E  = "e"
    show NE = "ne"
    show L  = "l"
    show LE = "le"
    show G  = "g"
    show GE = "ge"
    show Z  = "z"

data Scale = S2 | S4 | S8 -- possible scaling values for effective addressing
  deriving (Eq, Ord, Show)

scaleToInt :: Scale -> Int32
scaleToInt S2 = 2
scaleToInt S4 = 4
scaleToInt S8 = 8

data EffectiveAddress = EffectiveAddress
    { base         :: Maybe Temp
    , indexScale   :: Maybe (Temp, Scale)
    , displacement :: Int32
    }
  deriving (Eq, Ord)

data Operand = Imm Int32
             | Reg Temp
             | Mem EffectiveAddress
  deriving (Eq, Ord)

data X86Instr = Unary  UnaryInstr (Maybe SizeDirective, Operand)
              | Binary BinayInstr (Maybe SizeDirective, Operand)
                                  (Maybe SizeDirective, Operand)
              | LABEL Label
              | CDQ
              | JMP Label
              | J Cond Label
              | CALL Label
              | RET
              | NOP
  deriving (Eq, Ord)

data X86Comment = X86Comment String
  deriving (Eq, Ord)

comment :: String -> X86Comment
comment = X86Comment

emptyComment :: X86Comment
emptyComment = comment ""

instance Show X86Comment where
    show (X86Comment c) = "# " ++ c

data X86Func = X86Func {
    x86functionName :: String
  , x86body         :: [X86Instr]
  , x86comments     :: [X86Comment]
  , x86spilledCount :: Int32
  } deriving (Eq, Ord) 

data X86Prog = X86Prog {
    x86functions :: [X86Func]
  } deriving (Eq, Ord)

instance Show X86Prog where
    show p = concat ["\t.intel_syntax noprefix\n\t", ".global Lmain\n"]
          ++ concatMap (\x -> show x ++ "\n") (x86functions p)

instance Show X86Func where
    show f = x86functionName f ++ ":\n\t" ++
             concatMap (\x -> x ++ "\n\t") body 
      where

        body :: [String]
        body = zipWith (\x c -> printf "%-35s %s" (show x) (show c)) (x86body f) (x86comments f)

instance Show X86Instr where
    show (Unary  u (s,o))             = show u ++ maybe "" (\x -> ' ' : show x) s  ++ ' ' : show o
    show (Binary b (s1, o1) (s2, o2)) = show b ++ maybe "" (\x -> ' ' : show x) s1 ++ ' ' : show o1
                                        ++ ", "++ maybe "" (\x -> ' ' : show x) s2 ++ ' ' : show o2
    show (LABEL l)        = l ++ ":"
    show RET              = "ret"
    show CDQ              = "cdq"
    show (JMP l)          = "jmp " ++ l
    show (J cond l)       = "j" ++ show cond ++ " " ++ l
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
            (Just bt, _)           ->
                case displacement ea of
                     0 -> "[" ++ show bt ++ "]"
                     _ -> "[" ++ show bt                                     ++ " + " ++ c ++ "]"
            (_, _)                 -> "[" ++                                                         c ++ "]"


-- | Registers
--
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

ebx :: Operand
ebx = Reg ebxT

ebxT :: Temp
ebxT = mkNamedTemp "%ebx"


-- math register (used for mathematic expressions)
ecx :: Operand
ecx = Reg ecxT

ecxT :: Temp
ecxT = mkNamedTemp "%ecx"

-- used to move the first argument to edx if not already a register
edx :: Operand
edx = Reg edxT

edxT :: Temp
edxT = mkNamedTemp "%edx"

edi :: Operand
edi = Reg ediT

ediT :: Temp
ediT = mkNamedTemp "%edi"


esi :: Operand
esi = Reg esiT

esiT :: Temp
esiT = mkNamedTemp "%esi"
