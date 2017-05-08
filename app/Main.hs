module Main where

import BNF
import Lexer
import ParserLens        -- same functionality as ParserRecords
                         -- but with Lenses
-- import ParserRecords  --
import Text.Megaparsec

main :: IO ()
main = do
    let inputFiles = [
                      "examples/t0.sl"
                    , "examples/t1.sl"
                    , "examples/t2.sl"
                    , "examples/t3.sl"
                    , "examples/t4.sl"
                     ]
    mapM_ evaluateSLProgram inputFiles

evaluateSLProgram :: FilePath -> IO ()
evaluateSLProgram inputFile = do
    putStrLn $ ">> Starting to lex " ++ inputFile
    eitherres <- runParser stmListParser inputFile <$> readFile inputFile
    case eitherres of
        (Left errors) -> putStrLn $ ">> Errors while lexing: " ++ (show errors)
        (Right ast  ) -> do
            putStrLn $ ">> AST Output: "
            print ast
            putStrLn $ ">> Evaluating " ++ inputFile ++ ":"
            (_, memory) <- eval ast
            putStrLn "\n>> Memory dump:"
            print memory
            putStrLn $ ">> End of evaluation for " ++ inputFile ++ "\n"
