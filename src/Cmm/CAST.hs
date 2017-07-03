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
   | CJUMP  CmmRelOp CmmExp CmmExp Label Label
   | SEQ   [CmmStm]
   | LABEL  Label
   deriving Eq

data CmmBinOp =
     PLUS_C
   | MINUS_C
   | MUL_C
   | DIV_C
   | AND_C
   | OR_C
   | LSHIFT_C
   | RSHIFT_C
   | ARSHIFT_C
   | XOR_C
   deriving Eq

data CmmRelOp =
     EQ_C
   | NE_C
   | LT_C
   | GT_C
   | LE_C
   | GE_C
   | ULT_C
   | ULE_C
   | UGT_C
   | UGE_C
   deriving Eq


cmm2str :: Cmm -> String
cmm2str = intercalate "\n\n" . map show

-- | The following Show instances shold produce tree programs
--   that are parseable using tree2c.
--
instance Show CmmMethod where
  show m = cmmMethodName m ++ "(" ++ show (cmmArgLength m) ++ ") {\n  "
           ++ intercalate "\n  " (map show (cmmBody m))
           ++ "\n  return " ++ show (cmmReturn m)
           ++ "\n}\n"

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

instance Show CmmBinOp where
  
   show PLUS_C    = "PLUS"
   show MINUS_C   = "MINUS"
   show MUL_C     = "MUL"
   show DIV_C     = "DIV"
   show AND_C     = "AND"
   show OR_C      = "OR"
   show LSHIFT_C  = "LSHIFT"
   show RSHIFT_C  = "RSHIFT"
   show ARSHIFT_C = "ARSHIFT"
   show XOR_C     = "XOR"


instance Show CmmRelOp where
   show EQ_C = "EQ"
   show NE_C = "NE"
   show LT_C = "LT"
   show GT_C = "GT"
   show LE_C = "LE"
   show GE_C = "GE"
   show ULT_C = "ULT"
   show ULE_C = "ULE"
   show UGT_C = "UGT"
   show UGE_C = "UGE"
