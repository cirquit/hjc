{-# LANGUAGE BangPatterns #-}

module Main where

import AST
import ASTParser
import Lexer
import Output
import SymbolTable
import TypeCheck.TypeCheck

import Text.Megaparsec
import Control.Monad (mapM_)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

defaultConfig :: Config
defaultConfig = Config 
    {
      parse'      = True
    , showAst'    = False
    , showResult' = False
    , showTime'   = True
    , outputDir   = "../output"
    , typeErrLvl  = AllErrors -- FirstError -- Silently 
    }

-- run all examples
main :: IO ()
main = do
    let directory = "../examples"
    inputFiles <- filter (/= "../examples/z_ShouldFail") . map (\x -> directory </> x) <$> listDirectory directory
    (t, _) <- timeItT $ mapM_ (evaluateSLProgram defaultConfig) inputFiles
    putStrLn $ "Used " ++ show (t * 1e-4) ++ "s cpu-time." 

-- run single example
main' :: IO ()
main' = do
    let inputFiles = [ "../examples/Add.java" ]
    mapM_ (evaluateSLProgram defaultConfig) inputFiles 

-- run examples that should fail (logically, not lexically)
main'' :: IO ()
main'' = do
    let failedDir = "../examples/z_ShouldFail"
    failedInputFiles <- map (\x -> failedDir </> x) <$> listDirectory failedDir
    mapM_ (evaluateSLProgram defaultConfig) failedInputFiles


evaluateSLProgram :: Config -> FilePath -> IO ()
evaluateSLProgram config inputFile = do
    createDirectoryIfMissing True $ outputDir config
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

    putStrLn $ replicate 80 '-'