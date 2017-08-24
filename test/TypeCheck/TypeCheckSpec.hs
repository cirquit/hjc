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
                      "examples/Small/Add.java"
                    , "examples/Small/Arg.java"
                    , "examples/Small/Arithmetic.java"
                    , "examples/Small/ArrayAccess.java"
                    , "examples/Small/ArrSum.java"
                    , "examples/Small/BlockStatement.java"
                    , "examples/Small/Call.java"
                    , "examples/Small/CallThis.java"
                    , "examples/Small/Comparisons.java"
                    , "examples/Small/Effects.java"
                    , "examples/Small/Factorial.java"
                    , "examples/Small/Fields.java"
                    , "examples/Small/If.java"
                    , "examples/Small/IntOps.java"
                    , "examples/Small/Leet.java"
                    , "examples/Small/Loops.java"
                    , "examples/Small/Min.java"
                    , "examples/Small/OurArrSum.java"
                    , "examples/Small/Precedence.java"
                    , "examples/Small/Scope2.java"
                    , "examples/Small/Shadow1.java"
                    , "examples/Small/Shadow2.java"
                    , "examples/Small/ShortCutAnd.java"
                    , "examples/Small/ShortCutReturn.java"
                    , "examples/Small/Spill.java"
                    , "examples/Small/StackArgs.java"
                    , "examples/Small/Stck.java"
                    , "examples/Small/Sum.java"
                    , "examples/Small/TestEq.java"
                    , "examples/Small/TrivialClass.java"
                    , "examples/Small/VVST.java"
                    , "examples/Small/While.java"

                    , "examples/Large/BinarySearch.java"
                    , "examples/Large/BinaryTree.java"
                    , "examples/Large/Brainfuck.java"
                    , "examples/Large/BubbleSort.java"
                    , "examples/Large/Div.java"
                    , "examples/Large/E.java"
                    , "examples/Large/Fannkuch.java"
                    , "examples/Large/Fib.java"
                    , "examples/Large/FibL.java"
                    , "examples/Large/GameOfLifeAnim.java"
                    , "examples/Large/GameOfLife.java"
                    , "examples/Large/Graph.java"
                    , "examples/Large/Hanoi.java"
                    , "examples/Large/LinearSearch.java"
                    , "examples/Large/LinkedList.java"
                    , "examples/Large/MandelbrotAnim.java"
                    , "examples/Large/Mandelbrot.java"
                    , "examples/Large/ManyArgs.java"
                    , "examples/Large/Newton.java"
                    , "examples/Large/Pi.java"
                    , "examples/Large/PiSin.java"
                    , "examples/Large/Primes.java"
                    , "examples/Large/QuickSort.java"
                    , "examples/Large/RaytraceAndGauss.java"
                    , "examples/Large/Raytrace.java"
                    , "examples/Large/QuickSort.java"
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
