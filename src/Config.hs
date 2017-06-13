module Config where

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L

data TypeErrorLevel =
    Silently
    | FirstError
    | AllErrors
        deriving (Eq, Enum, Show)

data Config = Config {
    parse'        :: Bool
  , showAst'      :: Bool
  , showResult'   :: Bool
  , showTime'     :: Bool
  , compileToCmm  :: Bool
  , javaOutputDir :: FilePath
  , cmmOutputDir  :: FilePath
  , typeErrLvl    :: TypeErrorLevel
} deriving (Show)


data CmdParam = CmdParam {
    path        :: String
  , typeErrLvl' :: TypeErrorLevel
  , wholeDir    :: Bool
} deriving (Eq, Show)


cmdP :: Parser CmdParam
cmdP = do
    path <- identifier
    space
    errLvl <- option FirstError ((string "--tsilent" *> return Silently)
                       <|> (string "--tfirst" *> return FirstError)
                       <|> (string "--tall" *> return AllErrors))
    space
    wholeDir <- option False (return True <* string "--dir")
    return $ CmdParam path errLvl wholeDir

identifier :: Parser String
identifier = (:) <$> (alphaNumChar <|> char '.') <*> many (alphaNumChar <|> char '.' <|> char '/' <|> char '_')
