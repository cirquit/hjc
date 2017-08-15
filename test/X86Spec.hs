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
        describe "should be successful for small test" $ do
            let showTypeErrors      = False
                successfulTitle     = "should emulate "
                pathToTestFile      = "test/examples/MiniJava_Examples/Small/"
                successfulFileNames = [
                      "Add"
                    , "ArrayAccess"
                    , "ArrSum"
                    , "Effects"
                    , "Factorial"
                    , "Precedence"
                    , "Scope2"
                    , "ShortCutAnd"
                    , "Stck"
                    , "Sum"
                    , "TestEq"
                    , "TrivialClass"
                    , "While"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = pathToTestFile ++ input ++ ".java"
                let asmFile = ((("x86-output/") ++ input ++".s")::FilePath)
                run inputFile False
                result <- newMain asmFile
                removeFile asmFile
                result `shouldBe` (Just True)) testList

        describe "should be successful for small test" $ do
            let showTypeErrors      = True
                successfulTitle     = "should emulate "
                pathToTestFile      = "test/examples/MiniJava_Examples/Large/"
                successfulFileNames = [
                      "BinarySearch"
                    , "BinaryTree"
                    , "BubbleSort"
                    , "Fib"
                    , "FibL"
                    , "Graph"
                    , "LinearSearch"
                    , "LinkedList"
                    , "ManyArgs"
                    , "Newton"
                    , "Primes"
                    , "QuickSort"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = pathToTestFile ++ input ++ ".java"
                let asmFile = ((("x86-output/") ++ input ++".s")::FilePath)
                run inputFile False
                result <- newMain asmFile
                removeFile asmFile
                result `shouldBe` (Just True)) testList


testConfig :: Config
testConfig = Config
    {
      parse'        = True
    , showAst'      = False
    , showResult'   = False
    , showTime'     = True
    , compileToCmm  = True
    , compileToX86  = True
    , canonizeCmm   = False
    , javaOutputDir = "output"
    , cmmOutputDir  = "cmm-output"
    , x86OutputDir  = "x86-output"
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
