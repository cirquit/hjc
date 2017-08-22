module Config where

import Options.Applicative
import Data.Semigroup ((<>))

data TypeErrorLevel =
    Silently
    | FirstError
    | AllErrors
        deriving (Eq, Enum, Show, Read)

data Config = Config {
    javaFile      :: String
  , showAst'      :: Bool
  , showResult'   :: Bool
  , showTime'     :: Bool
  , canonizeCmm   :: Bool
  , compileToCmm  :: Bool
  , compileToX86  :: Bool
  , createCFGraph :: Bool
  , javaOutputDir :: String
  , cmmOutputDir  :: String
  , x86OutputDir  :: String
  , cfOutputDir   :: String
  , typeErrLvl    :: TypeErrorLevel
  , compileToAllocatedX86 :: Bool
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config {
    javaFile      = ""
  , showAst'      = False
  , showResult'   = False
  , showTime'     = True
  , canonizeCmm   = False
  , compileToCmm  = False
  , compileToX86  = False
  , createCFGraph = False
  , javaOutputDir = "../output"
  , cmmOutputDir  = "../cmm-output"
  , x86OutputDir  = "../x86-output"
  , cfOutputDir   = "../cf-graph-output"
  , typeErrLvl    = AllErrors
  , compileToAllocatedX86 = True
}

parseConfig :: Parser Config
parseConfig = Config
   <$> argument str (metavar "javaFile")
   <*> flag False True
       ( long "showAst"
       <> help "show Ast")
   <*> flag False True
       ( long "showResult"
       <> short 'r'
       <> help "show Result")
   <*> flag True False
       ( long "showTime"
       <> short 't'
       <> help "show Time")
   <*> flag True False
       ( long "canonCmm"
       <> help "canonize Cmm")
   <*> flag True False
       ( long "compileToCmm"
       <> short 'c'
       <> help "compile to Cmm")
   <*> flag False True
       ( long "compileToX86"
       <> short 'w'
       <> help "compile to X86 Code")
   <*> flag True False
       ( long "createCFGraph"
       <> short 'g'
       <> help "create Control Flow Graph")
   <*> strOption
       ( long "javaOutputDir"
       <> metavar "javaDir"
       <> value "output"
       <> help "directory of java files" )
   <*> strOption
       ( long "cmmOutputDir"
       <> metavar "cmmDir"
       <> value "cmm-output"
       <> help "directory of cmm files" )
   <*> strOption
       ( long "x86OutputDir"
       <> metavar "x86Dir"
       <> value "x86-output"
       <> help "directory of x86 assembly files" )
   <*> strOption
       ( long "cfOutputDir"
       <> metavar "graphDir"
       <> value "cf-graph-output"
       <> help "directory of control flow graph" )
   <*> typeErrorLvl
   <*> flag True False
       ( long "compileToAllocatedX86"
       <> short 'x'
       <> help "compile to allocated X86 Code")

typeErrorLvl :: Parser TypeErrorLevel
typeErrorLvl = option auto
            ( long "typeErrLvl"
           <> short 'e'
           <> metavar "[AllErrors|Silently|FirstError]"
           <> value AllErrors
           <> help "defines which level of errors should bee shown" )
