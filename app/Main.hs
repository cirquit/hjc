{-# LANGUAGE BangPatterns #-}

module Main where

import AST
import ASTParser
import Lexer
import Output
import SymbolTable
import TypeCheck.TypeCheck
import Config

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Control.Monad (mapM_, filterM)
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix ((</>))
import System.Environment

defaultConfig :: Config
defaultConfig = Config
    {
      parse'        = True
    , showAst'      = True 
    , showResult'   = False
    , showTime'     = True
    , compileToCmm  = True 
    , javaOutputDir = "../output"
    , cmmOutputDir  = "../cmm-output"
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
                    (t, _) <- timeItT $ mapM_ (evaluateSLProgram defaultConfig) inputFiles'
                    showTimeFin t
                False -> do
                    evaluateSLProgram defaultConfig $ path res
                    return ()

-- run single example
main' :: IO ()
main' = do
    let inputFiles = [ "../examples/Factorial.java" ]
    mapM_ (evaluateSLProgram defaultConfig) inputFiles 

-- run examples that should fail (logically, not lexically)
main'' :: IO ()
main'' = do
    let failedDir = "../examples/z_ShouldFail"
    failedInputFiles <- map (\x -> failedDir </> x) <$> listDirectory failedDir
    mapM_ (evaluateSLProgram defaultConfig) failedInputFiles


evaluateSLProgram :: Config -> FilePath -> IO ()
evaluateSLProgram config inputFile = do
    createDirectoryIfMissing True $ javaOutputDir config
    createDirectoryIfMissing True $ cmmOutputDir config
    putStrLn $ ">> Starting to lex " ++ inputFile
    (time, (eres, input)) <- timeItT $ do
        input <- readFile inputFile
        let !eitherres = runParser testParser inputFile input
        return (eitherres, input) 
    case eres of
        (Left errors) -> do
            let oi = failed inputFile input time errors
            showFailure oi config
            showTime oi config
        (Right ast) -> do 

            (ttime, typescope) <- timeItT $ typecheck ast

            let oi = success inputFile input (ttime + time) ast typescope

            -- std out
            showSuccess oi config
            showTypeScope oi config
            showTime oi config

            -- parsing ast to java
            writeJavaOutput oi config
            writeCmmOutput  oi config

    putStrLn $ replicate 80 '-'
