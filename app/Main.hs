module Main where

import BNF
import Lexer

import Text.Megaparsec

parser = stmParser

main :: IO ()
main = do
    input <- readFile "u01-input.txt"
    parseTest parser input
