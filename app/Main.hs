{-# LANGUAGE BangPatterns #-}

module Main where

import AST
import ASTParser
import Lexer
import Output

import Text.Megaparsec
import Control.Monad (mapM_)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))

defaultConfig :: Config
defaultConfig = Config 
    {
      parse'      = Just True
    , showAst'    = Just False 
    , showResult' = Just False
    , showTime'   = Just True
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
            let oi = success inputFile input time ast
            -- showAst ast config
            showSuccess oi config
            showTime oi config
            -- putStrLn $ replicate 80 '-'
    putStrLn $ replicate 80 '-'
