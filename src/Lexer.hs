module Lexer where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L

-- | space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | run the space consumer after every parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc 

-- | parse a symbol 
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = L.integer

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or", "print"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "hjc: keyword " ++ show x ++ " cannot be an identifier"
                else return x

comma :: Parser String
comma = symbol ","
