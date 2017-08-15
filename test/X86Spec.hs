module X86Spec (main, spec) where

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
import System.Process
import System.Exit


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "X86" $ do
        describe "should be successful for small test" $ do
            let showTypeErrors      = True
                successfulTitle     = "should run via risc386: "
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
                let inputFile = "test/examples/MiniJava_Examples/Small/" ++ input ++ ".java"
                run inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList -- (risc does it the other way around 1 ~ Failure, 0 ~ Success)

        describe "should be successful for large tests" $ do
            let showTypeErrors      = True
                successfulTitle     = "should have zero length of typescope errors in "
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
                let inputFile = "test/examples/MiniJava_Examples/Large/" ++ input ++ ".java"
                run inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList -- (risc does it the other way around 1 ~ Failure, 0 ~ Success)


makeC :: String -> IO ExitCode
makeC inputFile = do
    (_, Just hout, _, ph) <- createProcess (proc "risc386" [inputFile ++ ".s", ">", "/dev/null", "2>&1"]){ cwd = Just "x86-output", std_out = CreatePipe }
    eC <- waitForProcess ph
    -- (_, _, _, cleanH) <- createProcess (proc "rm" ["*.s"]){ cwd = Just "x86-output", std_out = CreatePipe }
    -- _ <- waitForProcess cleanH
    return eC


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
            -- writeCmmOutput  oi testConfig
            writeX86Output oi testConfig
            when showTypeErrors $ showErrors typescope
            return $ (0 < (length $ T._errors typescope))


showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
