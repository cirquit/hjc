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
    describe "ASTParser.testParser" $ do
        it "should have ast in Right of Arg.java" $ do
            let inputFile = "test/examples/Arg.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Arithmetic.java" $ do
            let inputFile = "test/examples/Arithmetic.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of BinarySearch.java" $ do
            let inputFile = "test/examples/BinarySearch.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of BinaryTree.java" $ do
            let inputFile = "test/examples/BinaryTree.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of BlockStatement.java" $ do
            let inputFile = "test/examples/BlockStatement.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Brainfuck.java" $ do
            let inputFile = "test/examples/Brainfuck.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of BubbleSort.java" $ do
            let inputFile = "test/examples/BubbleSort.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Call.java" $ do
            let inputFile = "test/examples/Call.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of CallThis.java" $ do
            let inputFile = "test/examples/CallThis.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Comparisons.java" $ do
            let inputFile = "test/examples/Comparisons.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Extends.java" $ do
            let inputFile = "test/examples/Extends.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Factorial.java" $ do
            let inputFile = "test/examples/Factorial.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Fields.java" $ do
            let inputFile = "test/examples/Fields.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of GameOfLifeAnim.java" $ do
            let inputFile = "test/examples/GameOfLifeAnim.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Hanoi.java" $ do
            let inputFile = "test/examples/Hanoi.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of If.java" $ do
            let inputFile = "test/examples/If.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of IntOps.java" $ do
            let inputFile = "test/examples/IntOps.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Leet.java" $ do
            let inputFile = "test/examples/Leet.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of LinearSearch.java" $ do
            let inputFile = "test/examples/LinearSearch.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of LinkedList.java" $ do
            let inputFile = "test/examples/LinkedList.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Loops.java" $ do
            let inputFile = "test/examples/Loops.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Min.java" $ do
            let inputFile = "test/examples/Min.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of QuickSort.java" $ do
            let inputFile = "test/examples/QuickSort.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of RaytraceAndGauss.java" $ do
            let inputFile = "test/examples/RaytraceAndGauss.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Raytrace.java" $ do
            let inputFile = "test/examples/Raytrace.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of Spill.java" $ do
            let inputFile = "test/examples/Spill.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of StackArgs.java" $ do
            let inputFile = "test/examples/StackArgs.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of TreeVisitor.java" $ do
            let inputFile = "test/examples/TreeVisitor.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

        it "should have ast in Right of While.java" $ do
            let inputFile = "test/examples/While.java"
            eres <- parseFile inputFile
            (isRight eres) `shouldBe` True

parseFile inputFile = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    return eres
