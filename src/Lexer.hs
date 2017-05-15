module Lexer where

import Control.Monad (void)
import Data.Functor.Identity
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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.integer

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do", "break"]
   ++ ["true","false"]
   ++ ["char", "int", "boolean", "int[]"] 
   ++ ["this", "new", "extends", "class", "public", "return"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

comma :: Parser String
comma = symbol ","

dot :: Parser String
dot = symbol "."
