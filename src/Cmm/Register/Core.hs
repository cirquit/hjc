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

import           AST                        (MiniJava())
import           Cmm.X86.InstrCore          -- (X86Prog(), X86CodeGen())
-- import           Cmm.Backend            
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

-- | Temp state
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
    let ns = Set.toAscList (nodes ig)
    foldl' (\g n -> Map.insert n defaultTempState g) Map.empty ns

-- | Reg monading utils
--
updateInterferenceGraph :: (MonadNameGen m, MonadIO m, MachineFunction f i, Ord i, Show i) => f -> Reg m ()
updateInterferenceGraph f = do
    interferenceGraph .= createInterferenceGraph f

getChildren :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
getChildren t = (filterSM isValidChild) =<< successorsM t

getColoredChildren :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
getColoredChildren t = (filterSM isColored) =<< successorsM t

successorsM :: (MonadNameGen m, MonadIO m) => Temp -> Reg m (Set Temp)
successorsM t = (\ig -> successors ig t) <$> (view interferenceGraph <$> get)

allNodes :: (MonadNameGen m, MonadIO m) => Reg m (Set Temp)
allNodes = nodes <$> (view interferenceGraph <$> get)

getOutDegree :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Int
getOutDegree t = Set.size <$> getChildren t

setSpilled ::  (MonadNameGen m, MonadIO m) => Temp -> Reg m ()
setSpilled t = tempStates %= Map.insert t Spilled

moveToStack :: (MonadNameGen m, MonadIO m) => Temp -> Reg m ()
moveToStack t = do
    tempStates %= Map.insert t Stack
    tempStack  %= (\s -> t : s)

onStack ::  (MonadNameGen m, MonadIO m) => Temp -> Reg m Bool
onStack t = t `hasState` (== Stack)

getFromStack :: (MonadNameGen m, MonadIO m) => Reg m (Maybe Temp)
getFromStack  = do
    stack <- view tempStack <$> get
    case take 1 stack of
        (t:_) -> do
             tempStates %= Map.insert t Clean
             tempStack  %= drop 1
             return $ Just t
        _     -> return $ Nothing

getAllSpilled :: (MonadNameGen m, MonadIO m) => Reg m (Set Temp)
getAllSpilled = tuplesToSet . filter isSpilled . Map.toAscList <$> (view tempStates <$> get)
    where 
        tuplesToSet :: (Ord k) => [(k,v)] -> Set k
        tuplesToSet = Set.fromAscList . map fst

        isSpilled :: (Temp, State) -> Bool
        isSpilled (t,s) = s == Spilled

resetAllSpilledTemps :: (MonadNameGen m, MonadIO m) => Reg m ()
resetAllSpilledTemps = do
    tempStates %= Map.map (\s -> if (s == Spilled) then Clean else s)

-- 
deleteSpilledTemps :: (MonadNameGen m, MonadIO m) => Set Temp -> Reg m ()
deleteSpilledTemps spilled = mapM_ (\s -> tempStates %= Map.delete s) spilled

-- after a spill, new temps are created, but the older ones shouldn't be recolored
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
hasState :: (MonadNameGen m, MonadIO m) => Temp -> (State -> Bool) -> Reg m Bool
hasState t isState = do
    ts <- view tempStates <$> get
    case (Map.lookup t ts) of
        (Just s)
          | isState s -> return True
        (Just _)      -> return False
        Nothing       -> error $ "Cmm.Register.Core.hasState: Temp " ++ show t ++ " does not exist in tempState: " ++ show ts

-- | invariant - the tempState map has to represent all available nodes
getColor :: (MonadNameGen m, MonadIO m) => Temp -> Reg m Temp
getColor t = do
    ts <- view tempStates <$> get
    case (Map.lookup t ts) of
        (Just (Colored c)) -> return c
        s@_                -> error $ "Cmm.RegisterAllocation.getColor: Temp " ++ show t ++ " does not have the state 'Colored', but: " ++ show s

-- | isValidChild is only called in the `Cmm.Register.Allocation.simplify` function to caluclate the outDegree
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
createTempMapping :: (MonadNameGen m, MonadIO m) => Reg m (Map Temp Temp)
createTempMapping = do
    ts <- view tempStates <$> get
    return $ foldl' go Map.empty (Map.toAscList ts)
  where
    go :: Map Temp Temp -> (Temp, State) -> Map Temp Temp
    go mapping (t, Colored color) = Map.insert t color mapping
    go mapping (t, s@_)           = error $ "Cmm.Register.Core.createTempMapping: Temp " ++ show t ++ " had an invalid state " ++ show s ++ ". It should be Colored"

printDebugMessage str = do 
    tstates <- view tempStates <$> get
    ts <- view tempStack <$> get

    liftIO $ putStrLn $ "\nDEBUG - " ++ str ++ "\n"
    liftIO $ putStrLn $ "!!! Tempstack: !!! "
    liftIO $ putStrLn $ unlines $ map show ts
    liftIO $ putStrLn $ "!!! Tempstates: !!!"
    liftIO $ putStrLn $ unlines $ map show $ Map.toAscList tstates



-- | monadic set utils
--
filterSM :: (Monad m, Ord a) => (a -> m Bool) -> Set a -> m (Set a)
filterSM f s = Set.fromList <$> (filterM f $ Set.toAscList s)

mapSM :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapSM f s = Set.fromList <$> (mapM f (Set.toAscList s))

mapSM_ :: (Monad m, Ord a, Ord b) => (a -> m b) -> Set a -> m ()
mapSM_ f s = mapSM f s >> return ()


-- none :: [a] -> Bool
none s = 0 == (Set.size s)

