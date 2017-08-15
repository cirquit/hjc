module Risc386Clone.IntelMain where

import Control.Monad

import Data.List -- find, isSuffixOf
import qualified Data.Map as Map

import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class

import Text.PrettyPrint

import Risc386Clone.Frame
import Risc386Clone.FrameIntel
import Risc386Clone.LexIntel
import Risc386Clone.ParseIntel
import Risc386Clone.StateIntel hiding (print)
import Risc386Clone.Util -- Pretty
import Risc386Clone.Wellformed

data Flag
  = Verbose
  | Help
    deriving (Eq, Show)

optDescrs :: [OptDescr Flag]
optDescrs =
  [ Option ['?'] ["help"]      (NoArg Help)    "show usage information"
  , Option ['v'] ["verbose"]   (NoArg Verbose) "be verbose"
  ]

usage :: IO a
usage = do
  exitFailure

-- exitOnError :: Either String b -> IO Maybe
exitOnError = either crash return where crash a = exitFailure

{- main:
  * parse IntelFrame list
  * search "*main" function
  * split frames into blocks IBlockFrame
  * convert frame list into a map
  * execute main function
 -}
-- newMain :: FilePath -> IO Maybe Bool
newMain prgFile = do
  input   <- readFile prgFile
  let frames = parse (alexScanTokens input)
  -- anyting that ends in "main" is considered the main function
  lmain <- do
    exitOnError $
      maybe (Left "no main function found") Right $
        -- find (\ l -> drop (length l - 4) l == "main") $
        find ("main" `isSuffixOf`) $
          map frameName frames

--   hPutStrLn stderr ("## Input:\n" ++ (render $ ppr frames))
  -- check instructions for well-formedness
  exitOnError $ wellformed frames

  let bfs = map iBlocksFrame frames
--   hPutStrLn stderr ("## Blocks:\n" ++ (render $ ppr bfs))

  let framemap =  Map.fromList $ map (\ f@(IBlockFrame l _ _) -> (l,f)) $ bfs
  let (result, output) = run framemap lmain
  exitOnError result
  return $ Just True
