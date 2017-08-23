{-# LANGUAGE BangPatterns #-}

module Main where

import           AST
import           ASTParser
import           Lexer
import           Output
import           TimeMonad                      (timeItT, time_, time, getTimeInMs, getTimed, io)
import           SymbolTable
import           TypeCheck.TypeCheck
import           Config

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer  as L

import qualified Options.Applicative    as OS
import           Data.Semigroup                 ((<>))

import           Control.Monad                  (mapM_, filterM)
import           System.Directory               (listDirectory, createDirectoryIfMissing, doesFileExist)
import           System.FilePath.Posix          ((</>))
import           System.Environment
import           Prelude                hiding  (error)

main :: IO ()
main = do
    mainConfig <- OS.execParser opts
    evaluateProgram mainConfig $ javaFile mainConfig
    where
        opts = OS.info (parseConfig OS.<**> OS.helper)
            ( OS.fullDesc
            <> OS.header "hjc - A MiniJava Compiler in Haskell" )

evaluateProgram :: Config -> FilePath -> IO ()
evaluateProgram config inputFile = do
    createDirectoryIfMissing True $ javaOutputDir config
    createDirectoryIfMissing True $ cmmOutputDir  config
    createDirectoryIfMissing True $ x86OutputDir  config
    createDirectoryIfMissing True $ cfOutputDir   config
    putStrLn $ ">> Starting to lex " ++ inputFile
    (parseTime, (eres, input)) <- timeItT $ do
        input <- readFile inputFile
        let !eitherres = runParser testParser inputFile input
        return (eitherres, input) 
    case eres of
        (Left errors) -> do
            let oi = failed inputFile input parseTime errors
            showFailure oi config
            showTime oi config

        (Right ast) -> do

            oi <- getTimed $ do
                typescope <- time $ typecheck ast
                let oi = success inputFile input parseTime ast typescope
                
                time_ $ showTypeScope oi config
                time_ $ showSuccess   oi config
                time_ $ writeJavaOutput         oi config
                time_ $ writeCmmOutput          oi config
                time_ $ writeX86Output          oi config
                time_ $ writeIFGraph            oi config
                time_ $ writeAllocatedX86Output oi config
                ms <- getTimeInMs
                return oi { timeS = parseTime + ms }

            showTime oi config

    putStrLn $ replicate 80 '-'

mainTestConfig :: Config
mainTestConfig = defaultConfig {
    javaFile      = ""
  , showAst'      = False
  , showTime'     = True
  , canonizeCmm   = True
  , compileToCmm  = True
  , compileToX86  = True
  , compileToAllocatedX86 = True
  , createIFGraph = False
  , javaOutputDir = "../output"
  , cmmOutputDir  = "../cmm-output"
  , x86OutputDir  = "../x86-output"
  , cfOutputDir   = "../cf-graph-output"
  , typeErrLvl    = AllErrors
  , showJava      = False
  , inParallel    = False
}

mainSmall :: FilePath -> IO ()
mainSmall fp = do
    let inputFile = "../examples/MiniJava_Examples/Small/" ++ fp ++ ".java"
    evaluateProgram mainTestConfig inputFile

mainLarge :: FilePath -> IO ()
mainLarge fp = do
    let inputFile = "../examples/MiniJava_Examples/Large/" ++ fp ++ ".java"
    evaluateProgram mainTestConfig inputFile

