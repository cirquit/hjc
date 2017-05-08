module BNF where


import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Text.Megaparsec
import Lexer

type StraightLineBNF = StmList

data StmList = StmList [Stm] -- stm; stm* 
    deriving (Eq)

instance Show StmList where
    show (StmList stmList) = unlines $ map show stmList

data Stm =
     AssignStm   String  Exp -- id := exp 
   | PrintStm    ExpList     -- "(" explist ")"
   deriving (Show, Eq)

data ExpList = ExpList [Exp] -- ","
   deriving (Show, Eq)

data Exp = Exp PrimExp [(BinOp, PrimExp)] 
   deriving (Show, Eq)

data PrimExp =
     IdExp   String
   | NumExp  Integer
   | EseqExp StmList Exp     -- "(" stmlist, exp ")" 
   deriving (Show, Eq)

data BinOp =
      PLUS
    | MINUS
    | TIMES
    | DIV
    deriving (Show, Eq)

stmListParser :: Parser StmList
stmListParser = do
    stmList <- stmParser `sepEndBy` semi
    return $ StmList stmList


stmParser :: Parser Stm
stmParser = assignStm <|> printStm

assignStm :: Parser Stm
assignStm = do 
    id <- identifier
    symbol ":=" <|> symbol "="
    exp <- expParser
    return $ AssignStm id exp

printStm  :: Parser Stm
printStm  = do
    symbol "print"
    expList <- parens expListParser
    return $ PrintStm expList


expListParser :: Parser ExpList
expListParser = ExpList <$> (expParser `sepBy` comma )

expParser :: Parser Exp
expParser = Exp <$> primExpParser <*> many tupParser
    where
        tupParser :: Parser (BinOp, PrimExp)
        tupParser = (,) <$> binOpParser <*> primExpParser

idExp :: Parser PrimExp
idExp = IdExp <$> identifier

numExp :: Parser PrimExp
numExp = NumExp <$> lexeme integer

eseqExp :: Parser PrimExp
eseqExp = parens $ do
      stmL <- stmListParser
      comma
      exp <- expParser
      return $ EseqExp stmL exp

primExpParser :: Parser PrimExp
primExpParser = numExp <|> idExp <|> eseqExp

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