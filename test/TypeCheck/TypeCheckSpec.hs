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
                      "test/examples/Add.java"
                    , "test/examples/Arg.java"
                    , "test/examples/Arithmetic.java"
                    , "test/examples/BinarySearch.java"
                    , "test/examples/Add.java"
                    , "test/examples/BinarySearch.java"
                    , "test/examples/BinaryTree.java"
                    , "test/examples/BlockStatement.java"
                    , "test/examples/Brainfuck.java"
                    , "test/examples/BubbleSort.java"
                    , "test/examples/Call.java"
                    , "test/examples/CallThis.java"
                    , "test/examples/Comparisons.java"
                    , "test/examples/Extends.java"
                    , "test/examples/Factorial.java"
                    , "test/examples/Fields.java"
                    , "test/examples/GameOfLifeAnim.java"
                    , "test/examples/Hanoi.java"
                    , "test/examples/If.java"
                    , "test/examples/IntOps.java"
                    , "test/examples/Leet.java"
                    , "test/examples/LinearSearch.java"
                    , "test/examples/LinkedList.java"
                    , "test/examples/Loops.java"
                    , "test/examples/Min.java"
                    , "test/examples/QuickSort.java"
                    , "test/examples/RaytraceAndGauss.java"
                    , "test/examples/Raytrace.java"
                    , "test/examples/Spill.java"
                    , "test/examples/StackArgs.java"
                    , "test/examples/TreeVisitor.java"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` False) testList

            describe "inheritence tests should be successful" $ do

                let successfulTitle     = "should have zero length of typescope errors in "
                    successfulFileNames = [
                          "test/examples/Inheritance/EulerOO.java"
                        , "test/examples/Inheritance/NewtonOO.java"
                        , "test/examples/Inheritance/PrimesOO.java"
                        , "test/examples/Inheritance/QuickSortOO.java"
                        , "test/examples/Inheritance/TreeVisitor.java"
                        , "test/examples/Inheritance/AnimalInheritance.java"
                        ]    
                    testList            = zip successfulFileNames (iterate id successfulTitle)

                mapM_ (\(input, title) -> it (title ++ input) $ do
                    hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` False) testList



        describe "failed examples" $ do
            let showTypeErrors      = False
                successfulTitle     = "should have some typescope errors in "
                successfulFileNames = [
                      "test/examples/z_ShouldFail/TypeErrors/MethodCallWrong.java"
                    , "test/examples/z_ShouldFail/TypeErrors/MethodWronglySpelled.java"
                    , "test/examples/z_ShouldFail/TypeErrors/LinkedListBUG.java"
                    , "test/examples/z_ShouldFail/TypeErrors/PrintFirstArg.java"
                    , "test/examples/z_ShouldFail/TypeErrors/Shadow.java"
                    , "test/examples/z_ShouldFail/TypeErrors/TooManyArguments.java"
                    , "test/examples/z_ShouldFail/TypeErrors/TypeOfArgWrong.java"
                    , "test/examples/z_ShouldFail/TypeErrors/UndeclaredType.java"
                    , "test/examples/z_ShouldFail/TypeErrors/UndeclaredVar1.java"
                    , "test/examples/z_ShouldFail/TypeErrors/UndeclaredVar2.java"
                    , "test/examples/z_ShouldFail/TypeErrors/WrongArgumentTypes.java"
                    , "test/examples/z_ShouldFail/TypeErrors/WrongArgumentTypes2.java"
                    , "test/examples/z_ShouldFail/TypeErrors/WrongTypeDeclared.java"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                hasTypeErrors input showTypeErrors >>= \b -> b `shouldBe` True) testList

testConfig :: Config
testConfig = defaultConfig {
      parse'        = True
    , showAst'      = False
    , showResult'   = False
    , showTime'     = True
    , compileToCmm  = True 
    , javaOutputDir = "../output"
    , cmmOutputDir  = "../cmm-output"
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
