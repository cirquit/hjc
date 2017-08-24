module ASTParserSpec (main, spec) where

import ASTParser

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.Megaparsec
import Data.Either


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "should be successful for small tests" $ do
        let successfulTitle = "should create AST for "
            pathToTestFile  = "examples/Small/"
            inputFiles      = [
                  "Add"
                , "Arg"
                , "Arithmetic"
                , "ArrayAccess"
                , "ArrSum"
                , "BlockStatement"
                , "Call"
                , "CallThis"
                , "Comparisons"
                , "Effects"
                , "Factorial"
                , "Fields"
                , "If"
                , "IntOps"
                , "Leet"
                , "Loops"
                , "Min"
                , "OurArrSum"
                , "Precedence"
                , "Scope2"
                , "Shadow1"
                , "Shadow2"
                , "ShortCutAnd"
                , "ShortCutReturn"
                , "Spill"
                , "StackArgs"
                , "Stck"
                , "Sum"
                , "TestEq"
                , "TrivialClass"
                , "VVST"
                , "While"
                ]

            testList = zip inputFiles (iterate id successfulTitle)

        mapM_ (\(input, title) -> it (title ++ input) $ do
            let inputFile = pathToTestFile ++ input ++ ".java"

            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True) testList


    describe "should be successful for large tests" $ do
        let successfulTitle = "should create AST for "
            pathToTestFile  = "examples/Large/"
            inputFiles      = [
                "BinarySearch"
                , "BinaryTree"
                , "Brainfuck"
                , "BubbleSort"
                , "Div"
                , "E"
                , "Fannkuch"
                , "Fib"
                , "FibL"
                , "GameOfLifeAnim"
                , "GameOfLife"
                , "Graph"
                , "Hanoi"
                , "LinearSearch"
                , "LinkedList"
                , "MandelbrotAnim"
                , "Mandelbrot"
                , "ManyArgs"
                , "Newton"
                , "Pi"
                , "PiSin"
                , "Primes"
                , "QuickSort"
                , "RaytraceAndGauss"
                , "Raytrace"
                , "QuickSort"
                ]

            testList = zip inputFiles (iterate id successfulTitle)

        mapM_ (\(input, title) -> it (title ++ input) $ do
            let inputFile = pathToTestFile ++ input ++ ".java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True) testList


parseFile inputFile = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    return eres
