module TypeCheck.TypeCheckSpec (main, spec) where

import AST
import ASTParser
import TypeCheck.TypeCheck
import qualified TypeCheck.TCCore as T
import Output (showTE)

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
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Arg.java" $ do
            let inputFile = "test/examples/Arg.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Arithmetic.java" $ do
            let inputFile = "test/examples/Arithmetic.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BinarySearch.java" $ do
            let inputFile = "test/examples/BinarySearch.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BinaryTree.java" $ do
            let inputFile = "test/examples/BinaryTree.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BlockStatement.java" $ do
            let inputFile = "test/examples/BlockStatement.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Brainfuck.java" $ do
            let inputFile = "test/examples/Brainfuck.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in BubbleSort.java" $ do
            let inputFile = "test/examples/BubbleSort.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Call.java" $ do
            let inputFile = "test/examples/Call.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in CallThis.java" $ do
            let inputFile = "test/examples/CallThis.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Comparisons.java" $ do
            let inputFile = "test/examples/Comparisons.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Extends.java" $ do
            let inputFile = "test/examples/Extends.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Factorial.java" $ do
            let inputFile = "test/examples/Factorial.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Fields.java" $ do
            let inputFile = "test/examples/Fields.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in GameOfLifeAnim.java" $ do
            let inputFile = "test/examples/GameOfLifeAnim.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Hanoi.java" $ do
            let inputFile = "test/examples/Hanoi.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in If.java" $ do
            let inputFile = "test/examples/If.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in IntOps.java" $ do
            let inputFile = "test/examples/IntOps.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Leet.java" $ do
            let inputFile = "test/examples/Leet.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in LinearSearch.java" $ do
            let inputFile = "test/examples/LinearSearch.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in LinkedList.java" $ do
            let inputFile = "test/examples/LinkedList.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Loops.java" $ do
            let inputFile = "test/examples/Loops.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Min.java" $ do
            let inputFile = "test/examples/Min.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in QuickSort.java" $ do
            let inputFile = "test/examples/QuickSort.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors RaytraceAndGauss.java" $ do
            let inputFile = "test/examples/RaytraceAndGauss.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Raytrace.java" $ do
            let inputFile = "test/examples/Raytrace.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in Spill.java" $ do
            let inputFile = "test/examples/Spill.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors in StackArgs.java" $ do
            let inputFile = "test/examples/StackArgs.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0

        -- it "should have zero length of typescope errors String.java" $ do
        --     let inputFile = "test/examples/String.java"
        --     eres <- parseFile inputFile
        --     typescope <- checkType eres
        --     showErrors typescope
        --     (length $ T._errors typescope) `shouldBe` 0

        it "should have zero length of typescope errors TreeVisitor.java" $ do
            let inputFile = "test/examples/TreeVisitor.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            showErrors typescope
            (length $ T._errors typescope) `shouldBe` 0


        it "should have typescope errors MethodCallWrong.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/MethodCallWrong.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors MethodWronglySpelled.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/MethodWronglySpelled.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors LinkedListBUG.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/LinkedListBUG.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors PrintFirstArg.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/PrintFirstArg.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors Shadow.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/Shadow.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors TooManyArguments.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/TooManyArguments.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors TypeOfArgWrong.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/TypeOfArgWrong.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors UndeclaredType.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/UndeclaredType.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors UndeclaredVar1.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/UndeclaredVar1.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors UndeclaredVar2.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/UndeclaredVar2.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors WrongArgumentTypes.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/WrongArgumentTypes.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors WrongArgumentTypes2.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/WrongArgumentTypes2.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

        it "should have typescope errors WrongTypeDeclared.java" $ do
            let inputFile = "test/examples/z_ShouldFail/TypeErrors/WrongTypeDeclared.java"
            eres <- parseFile inputFile
            typescope <- checkType eres
            (0 < (length $ T._errors typescope)) `shouldBe` True

parseFile inputFile = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    return eres

checkType (Right ast) = do
    typescope <- typecheck ast
    return typescope

showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
