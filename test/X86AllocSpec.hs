module X86AllocSpec (main, spec) where

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
import System.Directory
import System.Process
import System.Exit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "should be successful for small tests" $ do
            let showTypeErrors      = False
                successfulTitle     = "should compile with gcc "
                pathToTestFile      = "examples/MiniJava_Examples/Small/"
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
                let inputFile = "examples/MiniJava_Examples/Small/" ++ input ++ ".java"
                run inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList

        describe "should be successful for large tests" $ do
            let showTypeErrors      = True
                successfulTitle     = "should compile with gcc "
                pathToTestFile      = "examples/MiniJava_Examples/Large/"
                successfulFileNames = [
                  --   "BinarySearch"
                  -- , "BinaryTree"  -- takes too long for risc386
                     "BubbleSort" 
                    , "Fib"
                    , "FibL"
                  -- , "LinearSearch"
                  -- , "Graph"
                  -- , "LinkedList"
                    , "ManyArgs"
                   -- , "Newton"      -- takes too long for risc386
                    , "Primes"
                    , "QuickSort"
                   -- , "GameOfLife"  -- takes too long for risc386
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)
            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = "examples/MiniJava_Examples/Large/" ++ input ++ ".java"
                run inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList

testConfig :: Config
testConfig = Config
    {
      javaFile      = ""
    , showAst'      = False
    , showResult'   = False
    , showTime'     = True
    , compileToCmm  = False
    , compileToX86  = False
    , compileToAllocatedX86 = True
    , canonizeCmm   = True
    , createCFGraph = False 
    , javaOutputDir = "output"
    , cmmOutputDir  = "cmm-output"
    , x86OutputDir  = "x86-output"
    , cfOutputDir   = "cf-graph-output"
    , typeErrLvl   = AllErrors -- FirstError -- Silently 
    }

makeC :: String -> IO ExitCode
makeC inputFile = do
    (_, Just hout, _, ph) <- createProcess (proc "make" ["file=" ++ inputFile ++ "-allocated.s"]){ cwd = Just "x86-output", std_out = CreatePipe }
    eC <- waitForProcess ph
    (_, _, _, cleanH) <- createProcess (proc "make" ["clean"]){ cwd = Just "x86-output", std_out = CreatePipe }
    _ <- waitForProcess cleanH
    return eC

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
            writeAllocatedX86Output oi testConfig
            when showTypeErrors $ showErrors typescope
            return $ (0 < (length $ T._errors typescope))


showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
