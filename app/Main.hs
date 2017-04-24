module Main where

import BNF
import Lexer

import Text.Megaparsec

parser = binOp

main :: IO ()
main = do
    input <- readFile "input.txt"

    parseTest parser input
