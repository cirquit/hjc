{-# LANGUAGE BangPatterns #-}

module Main where

import           AST
import           ASTParser
import           Lexer
import           Output
import           TimeMonad                   (timeItT, time_, time, getTimeInMs, getTimed, io)
import           SymbolTable
import           TypeCheck.TypeCheck
import           Config

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer  as L

import           Control.Monad               (mapM_, filterM)
import           System.Directory            (listDirectory, createDirectoryIfMissing, doesFileExist)
import           System.FilePath.Posix       ((</>))
import           System.Environment

mainConfig :: Config
mainConfig = defaultConfig  {
      parse'        = True
    , showAst'      = False 
    , showResult'   = False
    , showTime'     = True
    , compileToCmm  = True
    , canonizeCmm   = True
    , compileToX86  = True
    , javaOutputDir = "../output"
    , cmmOutputDir  = "../cmm-output"
    , x86OutputDir  = "../x86-output"
    , typeErrLvl    = AllErrors -- FirstError -- Silently 
    }

-- run all examples
main :: IO ()
main = do
    x <- getArgs
    let eres = runParser cmdP "" (unwords x)
    case eres of
        (Left errors) -> do
            print errors
        (Right res) -> do
            let directory = path res
            case (wholeDir res) of
                True -> do
                    inputFiles <- map (\x -> directory </> x) <$> listDirectory directory
                    inputFiles' <- filterM doesFileExist inputFiles
                    (t, _) <- timeItT $ mapM_ (evaluateProgram defaultConfig) inputFiles'
                    showTimeFin t
                False -> do
                    evaluateProgram defaultConfig $ path res

-- run single example
main' :: FilePath -> IO ()
main' fp = do
    let inputFiles = [ "../examples/" ++ fp ++ ".java" ]
    mapM_ (evaluateProgram mainConfig) inputFiles 

-- run examples that should fail (logically, not lexically)
main'' :: IO ()
main'' = do
    let failedDir = "../examples/z_ShouldFail"
    failedInputFiles <- map (\x -> failedDir </> x) <$> listDirectory failedDir
    mapM_ (evaluateProgram defaultConfig) failedInputFiles


evaluateProgram :: Config -> FilePath -> IO ()
evaluateProgram config inputFile = do
    createDirectoryIfMissing True $ javaOutputDir config
    createDirectoryIfMissing True $ cmmOutputDir config
    createDirectoryIfMissing True $ x86OutputDir config
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

                -- std out
                time_ $ showTypeScope oi config
                time_ $ showSuccess   oi config

                -- parsing ast to java
                time_ $ writeJavaOutput oi config
                time_ $ writeCmmOutput  oi config
                time_ $ writeX86Output  oi config

                ms <- getTimeInMs
                return oi { timeS = parseTime + ms }

            showTime oi config

    putStrLn $ replicate 80 '-'