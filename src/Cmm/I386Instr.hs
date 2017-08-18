{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Cmm.I386Instr where

import Cmm.LabelGenerator
import Cmm.Backend               (MachineInstr(..), MachineFunction(..), MachinePrg(..))
import Data.Int
import Text.Printf
import Data.Maybe                (fromJust)
import           Data.Set        (Set) 
import qualified Data.Set as Set 
import           Debug.Trace     (trace)


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

--  use  :: i -> Set Temp
    use i                = Set.fromAscList $ x86Use i

--  def  :: i -> Set Temp
    def i                = Set.fromAscList $ x86Def i

--  isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
    isMoveBetweenTemps i = x86MovT i

--  isAssignmentToTemp :: i -> Maybe Temp
    isAssignmentToTemp i = x86AssignT i

--  jumps :: i -> [Label]
    jumps i              = x86Jump i

--  isFallThrough :: i -> Bool
    isFallThrough i      = x86FallThrough i

--  isLabel :: i -> Maybe Label
    isLabel i            = x86Label i

--  renameInstr :: i -> (Temp -> Temp) -> i
    renameInstr i f      = x86Rename i f

--  ret :: i
    ret = RET

addTemp :: Operand -> [Temp]
addTemp (Reg t)
  | espT == t = []
  | ebpT == t = []
  | otherwise = [t]
addTemp (Mem ea)
  | espT == t = []
  | ebpT == t = []
  | otherwise = [t]
  where
    t = fromJust $ base ea
addTemp _        = []

x86Use :: X86Instr -> [Temp]
x86Use (Unary  POP  (_, _))  = []
x86Use (Unary  PUSH (_, op)) = addTemp op
x86Use (Unary  IDIV (_, op)) = addTemp op
x86Use x@(Unary  _  (_, _))  = error $ "x86Use: didnt define rules for " ++ show x

x86Use x@(Binary MOV  (_, _) (_, op)) = addTemp op -- trace ("Use Mov for: " ++ show x ++ ", with temp " ++ show (addTemp op)) (addTemp op)
x86Use (Binary LEA  (_, _) (_, op)) = addTemp op

x86Use (Binary ADD  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary SUB  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary AND  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary OR   (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary XOR  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary CMP  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary IMUL (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use x@(Binary _  (_, _)   (_, _))   = error $ "x86Use: didnt define rules for " ++ show x

x86Use  RET                      = [eaxT]
x86Use x@_                       = [] -- trace ("No temps for this guy: " ++ show x) []


x86Def :: X86Instr -> [Temp]
x86Def (Unary  POP  (_, op)) = addTemp op
x86Def (Unary  PUSH (_, _))  = []
x86Def (Unary  IDIV (_, _))  = [edxT, eaxT]
x86Def x@(Unary _   (_, _))  = error $ "x86Def: didnt define rules for " ++ show x

x86Def x@(Binary MOV    (_, op) (_, _)) = addTemp op -- trace ("Def Mov for: " ++ show x ++ ", with temp " ++ show (addTemp op)) (addTemp op)
x86Def (Binary LEA    (_, op) (_, _)) = addTemp op
x86Def (Binary ADD    (_, op) (_, _)) = addTemp op
x86Def (Binary SUB    (_, op) (_, _)) = addTemp op
x86Def (Binary AND    (_, op) (_, _)) = addTemp op
x86Def (Binary OR     (_, op) (_, _)) = addTemp op
x86Def (Binary XOR    (_, op) (_, _)) = addTemp op
x86Def (Binary CMP    (_, _)  (_, _)) = []
x86Def (Binary IMUL   (_, op) (_, _)) = addTemp op
x86Def x@(Binary _    (_, op) (_, _)) = error $ "x86Def: didnt define rules for " ++ show x

x86Def (CALL _) = [eaxT]
x86Def x@_                         = [] -- trace ("No temps for this guy: " ++ show x) []



x86MovT (Binary _ (_, Reg t1) (_, Reg t2))
  | (t1 == ebpT) || (t1 == espT) || (t2 == ebpT) || (t2 == espT) = Nothing
  | otherwise                                            = Just (t1, t2)
x86MovT _                                                = Nothing

x86AssignT (Unary _  (_, (Reg t))   )  = Just t
x86AssignT (Binary _ (_, (Reg t1)) _)  = Just t1
x86AssignT _                           = Nothing


-- x86Jump (CALL l) = [l]
x86Jump (JMP l)  = [l]
x86Jump (J _ l)  = [l]
x86Jump _        = []


x86Label (LABEL l) = Just l
x86Label _         = Nothing


x86Rename (Unary i (s, Reg t)) f                 = Unary  i (s,  Reg (f t))
x86Rename (Unary i (s, Mem m)) f                 = Unary  i (s,  Mem (rMem f m))
x86Rename (Binary i (s1, Reg t1) (s2, Reg t2)) f = Binary i (s1, Reg (f t1)) (s2, Reg (f t2))
x86Rename (Binary i (s1, Mem m1) (s2, Mem m2)) f = Binary i (s1, Mem (rMem f m1)) (s2, Mem (rMem f m2))
x86Rename i _                                    = i

rMem f (EffectiveAddress (Just t1) (Just (t2, s)) d) = EffectiveAddress (Just (f t1)) (Just ((f t2), s)) d


x86FallThrough (RET)   = False
x86FallThrough (JMP _) = False
x86FallThrough _       = True




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

