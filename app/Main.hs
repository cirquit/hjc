module Main where

import AST
import ASTParser
import Lexer
import ParserLens        -- same functionality as ParserRecords
                         -- but with Lenses
-- import ParserRecords  --
import Text.Megaparsec
import Text.Megaparsec.Pos (Pos())
import Prelude hiding (head)
import Data.Set (toAscList)
import Rainbow
import Data.List.NonEmpty (head)
import System.Directory
import System.FilePath.Posix ((</>))

main :: IO ()
main = do
    let directory  = "../examples"
    inputFiles <- map (\x -> directory </> x) <$> listDirectory directory
    -- let inputFiles = [
    --                    "../examples/Add.java"
    --                  , "../examples/Arg.java"
    --                  , "../examples/Arithmetic.java"
    --                  , "../examples/BinarySearch.java"
    --                  , "../examples/Call.java"
    --                  ]
    mapM_ evaluateSLProgram inputFiles

evaluateSLProgram :: FilePath -> IO ()
evaluateSLProgram inputFile = do
    putStrLn $ ">> Starting to lex " ++ inputFile
    
    eitherres <- runParser testParser inputFile <$> readFile inputFile
    case eitherres of
        (Left errors) -> showErrors inputFile errors
        (Right ast  ) -> do
            putStrLn $ ">> AST Output: "
            print ast
            putStrLn $ ">> Successfully parsed " ++ inputFile
            putStrLn $ replicate 80 '-'

            -- putStrLn $ ">> Evaluating " ++ inputFile ++ ":"
            -- (_, memory) <- eval ast
            -- putStrLn "\n>> Memory dump:"
            -- print memory
            -- putStrLn $ ">> End of evaluation for " ++ inputFile ++ "\n"



showErrors :: FilePath -> ParseError Char Dec -> IO ()
showErrors fp (ParseError ps unexpected expected customHelp) = do
    input <- readFile fp 
    let fileLine = (lines input) !! (line - 1) 
    putStr   $ ">> " ++ fp ++ ":"
    putChunk $ (chunk line') <> (chunk (':':col')) & bold
    putStrLn $ ": error:"
    putStrLn $ ""
    putStrLn $ "  unexpected tokens: " ++ show (toAscList unexpected)
    putStrLn $ "    expected tokens: " ++ show (toAscList expected)
    putStrLn $ ""
    putStrLn $ "..."
    putStrLn $ "   " ++ fileLine
    putStrLn $ "..."
    putChunkLn $ (chunk "   ") <> chunk (concat (replicate (col-2) " ")) <> (chunk "^^^") & fore red & bold
    printHelp help
    putStrLn $ "  failed parsing."
    putStrLn $ replicate 80 '-'
       
  where
    line  = fromIntegral. unPos . sourceLine . head $ ps
    line' = show line
    col   = fromIntegral . unPos . sourceColumn . head $ ps
    col'  = show col

    help  = map toHelp (toAscList customHelp)

    toHelp (DecFail str) = str
    toHelp _             = ""

    printHelp list
        | null list = return ()
        | otherwise = putStrLn $ "  parse error: " ++ unlines list

    -- bold s = "\\e[1m" ++ s ++ "\\e[0m"
