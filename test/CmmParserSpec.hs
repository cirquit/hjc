module CmmParserSpec (main, spec) where

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
    describe "CmmParser" $ do
        describe "should be successful for small test" $ do
            let showTypeErrors      = True
                successfulTitle     = "should have zero length of typescope errors in "
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
                let inputFile = "examples/Small/" ++ input ++ ".java"
                hasTypeErrors inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList

        describe "should be successful for large test" $ do
            let showTypeErrors      = True
                successfulTitle     = "should have zero length of typescope errors in "
                pathToTestFile      = "examples/Large/"
                successfulFileNames = [
                      "BinarySearch"
                    , "BinaryTree"
                    , "Brainfuck"
                    , "BubbleSort"
                    , "Div"
                    , "E"
                    , "Fannkuch"
                    , "Fib"
                    , "FibL"
                    , "GameOfLifeAnim"
                    , "GameOfLife"
                    , "Graph"
                    , "Hanoi"
                    , "LinearSearch"
                    , "LinkedList"
                    , "MandelbrotAnim"
                    , "Mandelbrot"
                    , "ManyArgs"
                    , "Newton"
                    , "Pi"
                    , "PiSin"
                    , "Primes"
                    , "QuickSort"
                    , "RaytraceAndGauss"
                    , "Raytrace"
                    , "QuickSort"
                    ]

                testList            = zip successfulFileNames (iterate id successfulTitle)

            mapM_ (\(input, title) -> it (title ++ input) $ do
                let inputFile = "examples/Large/" ++ input ++ ".java"
                hasTypeErrors inputFile False
                makeC input >>= \b -> b `shouldBe` ExitSuccess) testList


makeC :: String -> IO ExitCode
makeC inputFile = do
    (_, Just hout, _, ph) <- createProcess (proc "make" ["file=" ++ inputFile ++ "-canonized.tree"]){ cwd = Just "cmm-output", std_out = CreatePipe }
    eC <- waitForProcess ph
    (_, _, _, cleanH) <- createProcess (proc "make" ["clean"]){ cwd = Just "cmm-output", std_out = CreatePipe }
    _ <- waitForProcess cleanH
    return eC


testConfig :: Config
testConfig = defaultConfig
    {
      javaFile      = ""
    , showAst'      = False
    , showTime'     = True
    , compileToCmm  = True
    , compileToX86  = False
    , compileToAllocatedX86 = False
    , canonizeCmm   = True 
    , createIFGraph = True
    , javaOutputDir = "output"
    , cmmOutputDir  = "cmm-output"
    , x86OutputDir  = "x86-output"
    , cfOutputDir   = "cf-graph-output"
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
            let oi = success inputFile input (0::Double) ast typescope
            writeCmmOutput  oi testConfig
            when showTypeErrors $ showErrors typescope
            return $ (0 < (length $ T._errors typescope))


showErrors :: T.TypeScope -> IO ()
showErrors typescope = do
    mapM_ (uncurry showTE) $ zip (T._errors typescope) [1..]
