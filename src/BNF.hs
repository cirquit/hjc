module BNF where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec
import Lexer

data Stm =
     ComboundStm Stm Stm
   | AssignStm String Stm
   | PrintStm [Exp]
   deriving Show

data Exp =
     IdExp String
   | NumExp Integer
   | OpExp Exp BinOp Exp
   | EseqExp Stm Exp
   deriving Show


-- idExp :: Parser Exp
-- idExp = symbol 

data BinOp =
      PLUS
    | MINUS
    | TIMES
    | DIV
    deriving Show

plusOp :: Parser BinOp
plusOp = symbol "+" *> return PLUS

minusOp :: Parser BinOp
minusOp = symbol "-" *> return MINUS

timesOp :: Parser BinOp
timesOp = symbol "*" *> return TIMES

divOp :: Parser BinOp
divOp = symbol "/" *> return DIV

binOp :: Parser BinOp
binOp = plusOp <|> minusOp <|> timesOp <|> divOp
