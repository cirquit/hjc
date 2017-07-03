module Output where

import           Text.Megaparsec
import           Text.Megaparsec.Pos                (Pos())
import           Rainbow

import           Prelude                     hiding (head)
import           System.CPUTime
import           System.FilePath.Posix              ((</>))
import           Data.Set                           (toAscList)
import           Data.List.NonEmpty                 (head)
import qualified Data.List.Split       as LS
import           Control.Lens
import           Control.Monad                      (when)

import           AST
import           Config
import           TypeCheck.TCCore                    (TypeScope(), TypeError(..), successful, errors)
import           Cmm.ASTToCmmParser                  (ast2cmms, ast2cmm)
import           Cmm.Canon                           (cmm2canons)

data OutputInfo = OutputInfo {
    fileName    :: String
  , fileInput   :: String
  , timeS       :: Double
  , ast         :: Maybe MiniJava
  , parseErrors :: Maybe (ParseError Char Dec)
  , typescope   :: Maybe TypeScope
}


success :: FilePath -> String -> Double -> MiniJava -> TypeScope -> OutputInfo
success fp i t ast ts = OutputInfo fp i t (Just ast) Nothing (Just ts)


failed :: FilePath -> String -> Double -> ParseError Char Dec -> OutputInfo
failed fp i t e       = OutputInfo fp i t Nothing    (Just e) Nothing


showTypeScope :: OutputInfo -> Config -> IO ()
showTypeScope oi config = do
    let (Just ts) = typescope oi
    putChunk (chunk ">> ")
    if (successful ts)
        then do 
            putChunk $ chunk "Successfully typechecked.\n" & fore green
        else do
            putChunk $ (chunk "Typechecking failed: \n\n") & fore red
            printErrors (view errors ts) (typeErrLvl config)

printErrors :: [TypeError] -> TypeErrorLevel -> IO ()
printErrors tes errlvl   = do
    let errCount = length tes
    let teErrStr
            | errCount == 1 = "typeerror"
            | otherwise     = "typeerrors"
    putChunk $ (chunk "        Terminating with ")
    putChunk $ (chunk $ show errCount) & bold
    putChunk $ (chunk $ " " ++ teErrStr ++ ".\n\n")
    case (null tes, errlvl) of
        (False, Silently  ) -> return ()
        (False, FirstError) -> showTE (tes !! 0) 1
        (False, AllErrors ) -> mapM_ (uncurry showTE) (zip tes [1..])
        (_, _)              -> return ()

showTE :: TypeError -> Int -> IO ()
showTE (TypeError c mm msg) i = do
    putChunk $ (chunk "   #") <> (chunk $ show i) & fore red & bold
    putChunk $ (chunk " class ")
    putChunk $ (chunk c) & bold
    showMethod mm
    putChunk $ (chunk "        ") <> (chunk msg) <> (chunk "\n\n")

showMethod :: Maybe Identifier -> IO ()
showMethod (Just mid) = do
    putChunk $ (chunk " : method ")
    putChunk $ (chunk mid) & bold
    putChunk $ (chunk ":") <> (chunk "\n") 
showMethod Nothing    = putChunk $ (chunk ":") <> (chunk "\n")

showSuccess :: OutputInfo -> Config -> IO ()
showSuccess (OutputInfo fp input _ (Just ast) _ _) config = do
    putChunk $ (chunk ">> ")
    putChunk $ (chunk "Successfully parsed: ") & fore green
    putChunk $ (chunk fp) & bold
    if (showAst' config)
        then print ast
        else return ()
    putChunkLn $ (chunk " (") <> (chunk . show . length . lines $ input) <> (chunk " lines)") & italic

showFailure :: OutputInfo -> Config -> IO ()
showFailure (OutputInfo fp _ _ _ (Just (ParseError ps unexpected expected customHelp)) _) config = do
    input <- readFile fp
    let fileLine = (lines input) !! (line - 1)
    putStr $ ">> " ++ fp ++ ":"
    putChunk $ (chunk line') <> (chunk (':' : col')) & bold
    putStrLn $ ": error:"
    putStrLn $ ""
    putStrLn $ "  unexpected tokens: " ++ show (toAscList unexpected)
    putStrLn $ "    expected tokens: " ++ show (toAscList expected)
    putStrLn $ ""
    putStrLn $ "..."
    putStrLn $ "   " ++ fileLine
    putStrLn $ "..."
    putChunkLn $
        (chunk "   ") <> chunk (concat (replicate (col - 2) " ")) <>
        (chunk "^^^") &
        fore red &
        bold
    printHelp help
    putStrLn $ "  failed parsing."
  where
    line = fromIntegral . unPos . sourceLine . head $ ps
    line' = show line
    col = fromIntegral . unPos . sourceColumn . head $ ps
    col' = show col
    help = map toHelp (toAscList customHelp)
    toHelp (DecFail str) = str
    toHelp _ = ""
    printHelp list
        | null list = return ()
        | otherwise = putStrLn $ "  parse error: " ++ unlines list


showTime :: OutputInfo -> Config -> IO ()
showTime oi config = do
    putChunk $ (chunk ">> Finished in: ") <> (chunk $ show (timeS oi)) <> (chunk "ms\n") & italic


showTimeFin :: Double -> IO ()
showTimeFin t = putChunk $ (chunk " Finished in: ") <> (chunk $ show t) <> (chunk "ms\n") & italic

-- |Wrap an 'IO' computation so that it returns execution time is seconds as well as the real value.
timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-8
    return (t, a)

writeJavaOutput :: OutputInfo -> Config -> IO ()
writeJavaOutput (OutputInfo inputName _ _ (Just ast) _ _) conf = do
    let _ : name : _ = reverse <$> (LS.splitOneOf "./" $ reverse inputName)
    let outputName = (javaOutputDir conf) </> (name ++ "-output.java")
    let result = showJC ast
    writeFile outputName result
    putChunk $ (chunk ">> ")
    putChunk $ (chunk "Written: ") & fore green
    putChunk $ (chunk outputName) <> (chunk "\n") & bold

writeCmmOutput :: OutputInfo -> Config -> IO ()
writeCmmOutput (OutputInfo inputName _ _ (Just ast) _ _) conf = when (compileToCmm conf) $ do
    let _ : name : _ = reverse <$> (LS.splitOneOf "./" $ reverse inputName)
    (outputName, result) <- do
        case (canonizeCmm conf) of
            True ->  do 
                let outputName = (cmmOutputDir conf) </> (name ++ "-canonized.tree")
                result <- cmm2canons =<< ast2cmm ast 
                return (outputName, result)
            False -> do
                let outputName = (cmmOutputDir conf) </> (name ++ ".tree")
                result <- ast2cmms ast
                return (outputName, result)
    writeFile outputName result
    putChunk $ (chunk ">> ")
    putChunk $ (chunk "Written: ") & fore green
    putChunk $ (chunk outputName) <> (chunk "\n") & bold