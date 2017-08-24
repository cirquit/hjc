module X86Spec (main, spec) where

import AST
import ASTParser
import TypeCheck.TypeCheck
import qualified TypeCheck.TCCore as T
import Output
import Config
import Risc386Clone.IntelMain

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad
import Text.Megaparsec
import System.Directory

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "should be successful for small tests" $ do
            let showTypeErrors      = False
                successfulTitle     = "should emulate "
                pathToTestFile      = "examples/Small/"
                successfulFileNames = [
                      "Add.java"
                    , "Arg.java"
                    , "Arithmetic.java"
                    , "ArrayAccess.java"
                    , "ArrSum.java"
                    , "BlockStatement.java"
                    , "Call.java"
                    , "CallThis.java"
                    , "Comparisons.java"
                    , "Effects.java"
                    , "Factorial.java"
                    , "Fields.java"
                    , "If.java"
                    , "IntOps.java"
                    , "Leet.java"
                    , "Loops.java"
                    , "Min.java"
                    , "OurArrSum.java"
                    , "Precedence.java"
                    , "Scope2.java"
                    , "Shadow1.java"
                    , "Shadow2.java"
                    , "ShortCutAnd.java"
                    , "ShortCutReturn.java"
                    , "Spill.java"
                    , "StackArgs.java"
                    , "Stck.java"
                    , "Sum.java"
                    , "TestEq.java"
                    , "TrivialClass.java"
                    , "VVST.java"
                    , "While.java"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = pathToTestFile ++ input
                let asmFile = ((("x86-output/") ++ input ++".s")::FilePath)
                run inputFile False
                result <- newMain asmFile
                removeFile asmFile
                result `shouldBe` (Just True)) testList

        describe "should be successful for large tests" $ do
            let showTypeErrors      = True
                successfulTitle     = "should emulate "
                pathToTestFile      = "examples/Large/"
                successfulFileNames = [
                      "BinarySearch.java"
--                     , "BinaryTree.java"       -- takes too long so far
                    , "Brainfuck.java"
                    , "BubbleSort.java"
                    , "Div.java"
                    , "E.java"
                    , "Fannkuch.java"
                    , "Fib.java"
                    , "FibL.java"
                    , "GameOfLifeAnim.java"
                    , "GameOfLife.java"
                    , "Graph.java"
                    , "Hanoi.java"
                    , "LinearSearch.java"
                    , "LinkedList.java"
                    , "MandelbrotAnim.java"
                    , "Mandelbrot.java"
                    , "ManyArgs.java"
                    , "Newton.java"
                    , "Pi.java"
                    , "PiSin.java"
                    , "Primes.java"
                    , "QuickSort.java"
--                     , "RaytraceAndGauss.java" -- takes too long so far
--                     , "Raytrace.java"         -- takes too long so far
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = pathToTestFile ++ input
                let asmFile = ((("x86-output/") ++ input ++".s")::FilePath)
                run inputFile False
                result <- newMain asmFile
                removeFile asmFile
                result `shouldBe` (Just True)) testList


testConfig :: Config
testConfig = defaultConfig
    {
      javaFile      = ""
    , showAst'      = False
    , showTime'     = True
    , compileToCmm  = True
    , compileToX86  = True
    , compileToAllocatedX86 = False
    , canonizeCmm   = True
    , createIFGraph = False
    , javaOutputDir = "output"
    , cmmOutputDir  = "cmm-output"
    , x86OutputDir  = "x86-output"
    , cfOutputDir   = "cf-graph-output"
    , typeErrLvl   = AllErrors -- FirstError -- Silently 
    }

run :: String -> Bool -> IO Bool
run inputFile showTypeErrors = do
    input <- readFile inputFile
    let eres = runParser testParser inputFile input
    case eres of
        (Left errors) -> do
            let oi = failed inputFile input (-1) errors
            showFailure oi testConfig
            return False
        (Right ast)   -> do
            typescope <- typecheck ast
            let oi = success inputFile input (0 :: Double) ast typescope
            writeX86Output oi testConfig
            when showTypeErrors $ showErrors typescope
            return $ (0 < (length $ T._errors typescope))


showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
