module TypeCheck.TypeCheckSpec (main, spec) where

import AST
import ASTParser
import TypeCheck.TypeCheck
import qualified TypeCheck.TCCore as T
import Output
import Config

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Text.Megaparsec


main :: IO ()
main = hspec spec





spec :: Spec
spec = do
    describe "TypeCheck.typecheck" $ do
        describe "should be successful" $ do
            let showTypeErrors      = True
                successfulTitle     = "should have zero length of typescope errors in "
                successfulFileNames = [
                      "examples/Arg.java"
                    , "examples/Arithmetic.java"
                    , "examples/Brainfuck.java"
                    , "examples/BlockStatement.java"
                    , "examples/Call.java"
                    , "examples/CallThis.java"
                    , "examples/Comparisons.java"
                    , "examples/Extends.java"
                    , "examples/Factorial.java"
                    , "examples/Fields.java"
                    , "examples/GameOfLifeAnim.java"
                    , "examples/Hanoi.java"
                    , "examples/If.java"
                    , "examples/IntOps.java"
                    , "examples/Leet.java"
                    , "examples/LinearSearch.java"
                    , "examples/LinkedList.java"
                    , "examples/Loops.java"
                    , "examples/Min.java"
                    , "examples/RaytraceAndGauss.java"
                    , "examples/Raytrace.java"
                    , "examples/Spill.java"
                    , "examples/StackArgs.java"
                    , "examples/TreeVisitor.java"
                    , "examples/MiniJava_Examples/Small/Add.java"
                    , "examples/MiniJava_Examples/Large/BubbleSort.java"
                    , "examples/MiniJava_Examples/Large/QuickSort.java"
                    , "examples/MiniJava_Examples/Large/BinarySearch.java"
                    , "examples/MiniJava_Examples/Large/BinaryTree.java"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` False) testList

            describe "inheritence tests should be successful" $ do

                let successfulTitle     = "should have zero length of typescope errors in "
                    successfulFileNames = [
                          "examples/Inheritance/EulerOO.java"
                        , "examples/Inheritance/NewtonOO.java"
                        , "examples/Inheritance/PrimesOO.java"
                        , "examples/Inheritance/QuickSortOO.java"
                        , "examples/Inheritance/TreeVisitor.java"
                        , "examples/Inheritance/AnimalInheritance.java"
                        ]    
                    testList            = zip successfulFileNames (iterate id successfulTitle)

                mapM_ (\(input, title) -> it (title ++ input) $ do
                    hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` False) testList



        describe "failed examples" $ do
            let showTypeErrors      = False
                successfulTitle     = "should have some typescope errors in "
                successfulFileNames = [
                      "examples/z_ShouldFail/TypeErrors/MethodCallWrong.java"
                    , "examples/z_ShouldFail/TypeErrors/MethodWronglySpelled.java"
                    , "examples/z_ShouldFail/TypeErrors/LinkedListBUG.java"
                    , "examples/z_ShouldFail/TypeErrors/PrintFirstArg.java"
                    , "examples/z_ShouldFail/TypeErrors/Shadow.java"
                    , "examples/z_ShouldFail/TypeErrors/TooManyArguments.java"
                    , "examples/z_ShouldFail/TypeErrors/TypeOfArgWrong.java"
                    , "examples/z_ShouldFail/TypeErrors/UndeclaredType.java"
                    , "examples/z_ShouldFail/TypeErrors/UndeclaredVar1.java"
                    , "examples/z_ShouldFail/TypeErrors/UndeclaredVar2.java"
                    , "examples/z_ShouldFail/TypeErrors/WrongArgumentTypes.java"
                    , "examples/z_ShouldFail/TypeErrors/WrongArgumentTypes2.java"
                    , "examples/z_ShouldFail/TypeErrors/WrongTypeDeclared.java"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` True) testList

testConfig :: Config
testConfig = defaultConfig {
      showAst'      = False
    , showTime'     = True
    , compileToCmm  = True
    , compileToAllocatedX86 = False
    , javaOutputDir = "output"
    , cmmOutputDir  = "cmm-output"
    , typeErrLvl   = AllErrors -- FirstError -- Silently 
    }

hasTypeErrors :: String -> Bool -> IO Bool
hasTypeErrors inputFile showTypeErrors = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    case eres of
        (Left errors) -> do
            let oi = failed inputFile input (-1) errors
            showFailure oi testConfig
            return False
        (Right ast)   -> do
            typescope <- typecheck ast
            when showTypeErrors $ showErrors typescope
            return $ (0 < (length $ T._errors typescope))


showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
