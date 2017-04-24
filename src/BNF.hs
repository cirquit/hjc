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

comboundStm :: Parser Stm
comboundStm = parens $ do
        stm1 <- stmParser
        symbol ","
        stm2 <- stmParser
        return $ ComboundStm stm1 stm2

assignStm :: Parser Stm
assignStm = do
    id <- identifier
    symbol ":=" 
    stm <- stmParser
    symbol ";"
    return $ AssignStm id stm

printStm :: Parser Stm
printStm = do
    symbol "print"
    expList <- manyTill expParser (symbol " ")
    return $ PrintStm expList

stmParser :: Parser Stm
stmParser = comboundStm <|> assignStm <|> printStm

data Exp =
     IdExp String
   | NumExp Integer
   | OpExp Exp BinOp Exp
   | EseqExp Stm Exp
   deriving Show

idExp :: Parser Exp
idExp = do
    id <- identifier
    return $ IdExp id

numExp :: Parser Exp
numExp = do
    int <- integer
    return $ NumExp int

opExp :: Parser Exp
opExp = do
    exp1 <- expParser
    binOp <- binOpParser
    exp2 <- expParser
    return $ OpExp exp1 binOp exp2

eseqExp :: Parser Exp
eseqExp = do
    stm <- stmParser
    exp <- expParser
    return $ EseqExp stm exp

expParser :: Parser Exp
expParser = idExp <|> numExp <|> opExp <|> eseqExp

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

binOpParser :: Parser BinOp
binOpParser = plusOp <|> minusOp <|> timesOp <|> divOp
