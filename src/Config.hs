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
  , canonizeCmm   :: Bool
  , compileToCmm  :: Bool
  , compileToX86  :: Bool
  , javaOutputDir :: FilePath
  , cmmOutputDir  :: FilePath
  , x86OutputDir  :: FilePath
  , typeErrLvl    :: TypeErrorLevel
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    parse'        = True
  , showAst'      = False
  , showResult'   = False
  , showTime'     = True
  , canonizeCmm   = False
  , compileToCmm  = False
  , compileToX86  = False
  , javaOutputDir = "output"
  , cmmOutputDir  = "cmm-output"
  , x86OutputDir  = "x86-output"
  , typeErrLvl    = AllErrors
}


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



