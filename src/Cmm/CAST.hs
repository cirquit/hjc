module Cmm.CAST where

import Prelude hiding (EQ,GT,LT)
import Data.List
import Data.Int
import Cmm.LabelGenerator

type Cmm = [CmmMethod]

data CmmMethod = CmmMethod
    { cmmMethodName ::  String
    , cmmArgLength  ::  Int
    , cmmBody       :: [CmmStm]
    , cmmReturn     ::  Temp
    }

data CmmExp =
     CONST Int32
   | NAME  Label
   | TEMP  Temp
   | PARAM Integer
   | BINOP CmmBinOp CmmExp CmmExp
   | MEM   CmmExp
   | CALL  CmmExp [CmmExp]
   | ESEQ  CmmStm CmmExp
   deriving Eq

data CmmStm =
     MOVE   CmmExp  CmmExp
   | JUMP   CmmExp [Label]
   | CJUMP  CmmRelOp   CmmExp CmmExp Label Label
   | SEQ   [CmmStm]
   | LABEL  Label
   deriving Eq

data CmmBinOp =
     PLUS
   | MINUS
   | MUL
   | DIV
   | AND
   | OR
   | LSHIFT
   | RSHIFT
   | ARSHIFT
   | XOR
   deriving (Eq, Show)

data CmmRelOp =
     EQ
   | NE
   | LT
   | GT
   | LE
   | GE
   | ULT
   | ULE
   | UGT
   | UGE
   deriving (Eq, Show)


cmm2str :: Cmm -> String
cmm2str = intercalate "\n\n" . map show

-- | The following Show instances shold produce tree programs
--   that are parseable using tree2c.
--
instance Show CmmMethod where
  show m = cmmMethodName m ++ "(" ++ show (cmmArgLength m) ++ ") {\n  "
           ++ intercalate "\n  " (map show (cmmBody m))
           ++ "\n  return " ++ show (cmmReturn m)
           ++ "\n}"

instance Show CmmExp where
  show (CONST i) = "CONST(" ++ show i ++ ")"
  show (NAME l) = "NAME(" ++ l ++ ")"
  show (TEMP t) = "TEMP(" ++ show t ++ ")"
  show (PARAM i) = "PARAM(" ++ show i ++ ")"
  show (BINOP o e1 e2) = "BINOP(" ++ show o ++ ", "
                                  ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (MEM  e) = "MEM(" ++ show e ++ ")"
  show (CALL e es) = "CALL(" ++ intercalate ", " (map show (e:es)) ++ ")"
  show (ESEQ s e) = "ESEQ(" ++ show s ++ ", " ++ show e ++ ")"

instance Show CmmStm where
  show (MOVE e1 e2) = "MOVE(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (JUMP e ls) = "JUMP(" ++ show e ++ ", " ++ intercalate ", " ls ++ ")"
  show (CJUMP r e1 e2 l1 l2) = "CJUMP(" ++ show r ++ ", "
                                        ++ show e1 ++ ", " ++ show e2 ++ ", "
                                        ++ l1 ++ ", " ++ l2 ++ ")"
  show (SEQ ss) = "SEQ(" ++ intercalate ", " (map show ss) ++ ")"
  show (LABEL l1) = "LABEL(" ++ l1 ++ ")"
