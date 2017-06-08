module TypeCheck.TypeCheckSpec (main, spec) where

import ASTParser
import TypeCheck.TypeCheck
import qualified TypeCheck.TCCore as T

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Text.Megaparsec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TypeCheck.typecheck" $ do
        it "should have zero length of typescope errors in Add.java" $ do
            let inputFile = "test/examples/Add.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Arg.java" $ do
            let inputFile = "test/examples/Arg.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Arithmetic.java" $ do
            let inputFile = "test/examples/Arithmetic.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BinarySearch.java" $ do
            let inputFile = "test/examples/BinarySearch.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BinaryTree.java" $ do
            let inputFile = "test/examples/BinaryTree.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BlockStatement.java" $ do
            let inputFile = "test/examples/BlockStatement.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Brainfuck.java" $ do
            let inputFile = "test/examples/Brainfuck.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BubbleSort.java" $ do
            let inputFile = "test/examples/BubbleSort.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Call.java" $ do
            let inputFile = "test/examples/Call.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in CallThis.java" $ do
            let inputFile = "test/examples/CallThis.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Comparisons.java" $ do
            let inputFile = "test/examples/Comparisons.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Extends.java" $ do
            let inputFile = "test/examples/Extends.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Factorial.java" $ do
            let inputFile = "test/examples/Factorial.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Fields.java" $ do
            let inputFile = "test/examples/Fields.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in GameOfLifeAnim.java" $ do
            let inputFile = "test/examples/GameOfLifeAnim.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Hanoi.java" $ do
            let inputFile = "test/examples/Hanoi.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in If.java" $ do
            let inputFile = "test/examples/If.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in IntOps.java" $ do
            let inputFile = "test/examples/IntOps.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Leet.java" $ do
            let inputFile = "test/examples/Leet.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in LinearSearch.java" $ do
            let inputFile = "test/examples/LinearSearch.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in LinkedList.java" $ do
            let inputFile = "test/examples/LinkedList.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Loops.java" $ do
            let inputFile = "test/examples/Loops.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Min.java" $ do
            let inputFile = "test/examples/Min.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in QuickSort.java" $ do
            let inputFile = "test/examples/QuickSort.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors RaytraceAndGauss.java" $ do
            let inputFile = "test/examples/RaytraceAndGauss.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Raytrace.java" $ do
            let inputFile = "test/examples/Raytrace.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Spill.java" $ do
            let inputFile = "test/examples/Spill.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in StackArgs.java" $ do
            let inputFile = "test/examples/StackArgs.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors String.java" $ do
            let inputFile = "test/examples/String.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors TreeVisitor.java" $ do
            let inputFile = "test/examples/TreeVisitor.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors While.java" $ do
            let inputFile = "test/examples/While.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (length $ T._errors typescope) `shouldBe` 0


parseFile inputFile = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    return eres

checkType (Right ast) = do
    typescope <- typecheck ast
    return typescope
