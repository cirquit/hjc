{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Cmm.Register.Core where 

import           Data.Set               (Set)
import qualified Data.Set        as Set
import           Data.Map               (Map)
import qualified Data.Map.Strict as Map

import           Text.Printf                (printf)
import           Data.List                  (foldl', find, maximumBy)
import           Debug.Trace                (trace)
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (comparing)
import           Control.Monad.Trans.State  hiding (State)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.IO.Class
import           Control.Lens               hiding ((#))
import           Control.Monad              (when, zipWithM_, filterM, mapM)
import           System.Random              (randomRIO)

import           AST                        (MiniJava())
import           Cmm.X86.InstrCore
import           Cmm.DirectedGraph
import           Cmm.InterferenceGraph      (createInterferenceGraph)
import           Cmm.Backend                (MachineInstr(..)
                                           , MachineFunction(..)
                                           , MachinePrg(..))
import           Cmm.LabelGenerator     

type Reg m = StateT RegisterState m

data RegisterState = RegisterState
    {  _interferenceGraph :: DirectedGraph Temp   -- graph with all connections between Temps
    ,  _tempStates        :: Map Temp State       -- using a map to represent the different states, because we don't want to modify the graph
    ,  _colors            :: Set Temp             -- colors are unique temps
    ,  _tempStack         :: [Temp]               -- using a list for representing a stack interface (saving the insertion order) 
    } deriving Show

interferenceGraph   :: Lens' RegisterState (DirectedGraph Temp)
interferenceGraph = lens _interferenceGraph (\x y -> x { _interferenceGraph = y })

tempStates   :: Lens' RegisterState (Map Temp State)
tempStates = lens _tempStates (\x y -> x { _tempStates = y })

colors   :: Lens' RegisterState (Set Temp)
colors = lens _colors (\x y -> x { _colors = y })

tempStack   :: Lens' RegisterState [Temp]
tempStack = lens _tempStack (\x y -> x { _tempStack = y })

-- | All possible Temp state
-- 
data State =
      Colored Temp -- colors are already the Temps with the named registers
    | Spilled         
    | Clean
    | Stack
  deriving (Ord, Show, Eq)

defaultTempState :: State
defaultTempState = Clean

createDefaultTempStates :: DirectedGraph Temp -> Map Temp State
createDefaultTempStates ig = do
    let ns = Set.toList (nodes ig)
    foldl' (\g n -> Map.insert n defaultTempState g) Map.empty ns

-- | Reg monad utils
--
-- |
updateInterferenceGraph :: (MonadNameGen m, MonadIO m, MachineFunction f i, Ord i, Show i) => f -> Reg m ()
updateInterferenceGraph f = do
    interferenceGraph .= createInterferenceGraph f

-- | get all successors, colored or not
--
getChildren :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
getChildren t = (filterSM isValidChild) =<< successorsM t

-- | used to pick a color which no successive child has
--
getColoredChildren :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
getColoredChildren t = (filterSM isColored) =<< successorsM t

-- |
successorsM :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
successorsM t = (\ig -> successors ig t) <$> (view interferenceGraph <$> get)

-- |
allNodes :: (MonadNameGen m, MonadIO m) => Reg m (Set Temp)
allNodes = nodes <$> (view interferenceGraph <$> get)

-- | dependent on the successors in the interference graph
--
getOutDegree :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Int
getOutDegree t = Set.size <$> getChildren t

-- | State abstraction for use with the interference graph
--   we use a map with a Temp -> State mapping to record changes
--   and apply to the graph afterwards
--
-- |
setSpilled ::  (MonadNameGen m, MonadIO m) => Temp -> Reg m ()
setSpilled t = tempStates %= Map.insert t Spilled

-- |
moveToStack :: (MonadNameGen m, MonadIO m) => Temp -> Reg m ()
moveToStack t = do
    tempStates %= Map.insert t Stack
    tempStack  %= (\s -> t : s)

-- |
onStack ::  (MonadNameGen m, MonadIO m) => Temp -> Reg m Bool
onStack t = t `hasState` (== Stack)

-- |
getFromStack :: (MonadNameGen m, MonadIO m) => Reg m (Maybe Temp)
getFromStack  = do
    stack <- view tempStack <$> get
    case take 1 stack of
        (t:_) -> do
             tempStates %= Map.insert t Clean
             tempStack  %= drop 1
             return $ Just t
        _     -> return $ Nothing

-- |
getAllSpilled :: (MonadNameGen m, MonadIO m) => Reg m (Set Temp)
getAllSpilled = tuplesToSet . filter isSpilled . Map.toList <$> (view tempStates <$> get)
    where 
        tuplesToSet :: (Ord k) => [(k,v)] -> Set k
        tuplesToSet = Set.fromList . map fst

        isSpilled :: (Temp, State) -> Bool
        isSpilled (t,s) = s == Spilled

-- | after spilling, if not all nodes are spilled, the leftover ones should be reset
--
resetAllSpilledTemps :: (MonadNameGen m, MonadIO m) => Reg m ()
resetAllSpilledTemps = do
    tempStates %= Map.map (\s -> if (s == Spilled) then Clean else s)

-- | after spilling, the spilled nodes should be deleted
--
deleteSpilledTemps :: (MonadNameGen m, MonadIO m) => Set Temp -> Reg m ()
deleteSpilledTemps spilled = mapM_ (\s -> tempStates %= Map.delete s) spilled

-- | after a spill, new temps are created and only those have to get
--   a Clean state, the others should remain colored
addNewTempStates :: (MonadNameGen m, MonadIO m) => Reg m ()
addNewTempStates = do
    ans       <- allNodes
    prevNodes <- Map.keysSet <$> (view tempStates <$> get)
    let newTemps = Set.filter (`Set.notMember` prevNodes) ans
    mapM_ (\t -> tempStates %= Map.insert t Clean) newTemps

isColored :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Bool
isColored t = t `hasState` colored
    where
        colored (Colored _) = True
        colored _           = False

-- | invariant - the tempState map has to represent all available nodes
--
hasState :: (MonadNameGen m, MonadIO m) => Temp -> (State -> Bool) -> Reg m Bool
hasState t isState = do
    ts <- view tempStates <$> get
    case (Map.lookup t ts) of
        (Just s)
          | isState s -> return True
        (Just _)      -> return False
        Nothing       -> error $ "Cmm.Register.Core.hasState: Temp " ++ show t ++ " does not exist in tempState: " ++ show ts

-- | invariant - the tempState map has to represent all available nodes
--
getColor :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Temp
getColor t = do
    ts <- view tempStates <$> get
    case (Map.lookup t ts) of
        (Just (Colored c)) -> return c
        s@_                -> error $ "Cmm.RegisterAllocation.getColor: Temp " ++ show t ++ " does not have the state 'Colored', but: " ++ show s

-- | isValidChild is only called in the `Cmm.Register.Allocation.simplify` function to caluclate the outDegree
--
isValidChild :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Bool
isValidChild t = do
    ts <- view tempStates <$> get
    case (Map.lookup t ts) of
        (Just Clean      ) -> return True
        (Just (Colored _)) -> return True
        (Just Stack      ) -> return False
        (Just Spilled    ) -> error "Cmm.Register.Core.isValidChild: There should never be any spilled nodes in the 'simplify' function"
        Nothing            -> error $ "Cmm.Register.Core.isValidChild: Could not find node " ++ show t ++ " in tempStates" 

-- | createTempMapping is only called when the coloring succeded and we are about to rename all temps in the function
--
createTempMapping :: (MonadNameGen m, MonadIO m) => Reg m (Map Temp Temp)
createTempMapping = do
    ts <- view tempStates <$> get
    return $ foldl' go Map.empty (Map.toAscList ts)
  where
    go :: Map Temp Temp -> (Temp, State) -> Map Temp Temp
    go mapping (t, Colored color) = Map.insert t color mapping
    go mapping (t, s@_)           = error $ "Cmm.Register.Core.createTempMapping: Temp " ++ show t ++ " had an invalid state " ++ show s ++ ". It should be Colored"

-- | Debug tool for the state
--
printDebugMessage :: (MonadNameGen m, MonadIO m) => String -> Reg m ()
printDebugMessage str = do 
    tstates <- view tempStates <$> get
    ts <- view tempStack <$> get
    liftIO $ putStrLn $ "\nDEBUG - " ++ str ++ ":\n"
    liftIO $ putStrLn $ "    Tempstack:\n"
    liftIO $ putStrLn $ unlines $ map show ts
    liftIO $ putStrLn $ "    Tempstates:\n"
    liftIO $ putStrLn $ unlines $ map show $ Map.toList tstates

-- | a "heuristic" to color the interference graph, somehow works better than without it
--
reverseStack :: (MonadNameGen m, MonadIO m) => Reg m ()
reverseStack = do
    i <- liftIO $ (randomRIO (1,2) :: IO Int)
    case (i - 1) of
        1 -> tempStack %= reverse
        _ -> return ()

-- | monadic set utils
--
filterSM :: (Monad m, Ord a) => (a -> m Bool) -> Set a -> m (Set a)
filterSM f s = Set.fromList <$> (filterM f $ Set.toList s)

mapSM :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapSM f s = Set.fromList <$> (mapM f (Set.toList s))

mapSM_ :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set a -> m ()
mapSM_ f s = mapSM f s >> return ()

none :: Set a -> Bool
none s = 0 == (Set.size s)