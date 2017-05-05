module Main where

import BNF
import Lexer
import Parser
import Text.Megaparsec

parser = stmListParser

main :: IO ()
main = do
    eitherres <- runParser parser "u01-input.txt" <$> readFile "u01-input.txt"
    case eitherres of
        (Left errors) -> putStrLn $ "errors while lexing: " ++ (show errors)
        (Right ast  ) -> do
            (nums, ram) <- eval ast
            putStrLn $ "nums: " ++ show nums
            putStrLn $ "ram: " ++ show ram
    return ()
