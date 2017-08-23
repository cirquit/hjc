{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Cmm.InterferenceGraph(
    createInterferenceGraph
    ) where 

import           Cmm.DirectedGraph
import           Cmm.Backend            (MachineInstr(..)
                                       , MachineFunction(..)
                                       , MachinePrg(..))
import           Cmm.LabelGenerator     (Temp())

import           Cmm.ControlFlowGraph   (createControlFlowGraph, Unique(..))
import           Cmm.ActivityAnalysis   (activityAnalysis, ActivityStorage(..))

import           Data.Set               (Set)
import qualified Data.Set        as Set
import           Data.Map               (Map)
import qualified Data.Map.Strict as Map

import           Text.Printf            (printf)
import           Data.List              (foldl', find)
import           Debug.Trace            (trace)
import           Data.Maybe             (fromJust)

createInterferenceGraph ::
  (MachineFunction f i, Ord i, Show i)
  => f
  -> DirectedGraph Temp
createInterferenceGraph function = do
    let -- cfgraph      :: DirectedGraph (Unique i)
        cfgraph = createControlFlowGraph function
        -- activity     :: Map (Unique i) ActivityStorage
        activity = activityAnalysis cfgraph
        -- instructions :: Set (Unique i)
        instructions =  nodes cfgraph
        -- states       :: (Map (Unique i) ActivityStorage, Set (Unique i))
        states = (activity, instructions)

    singleInterferenceGraph states

  where
    -- | this is just the implemented algorithm from the slides
    singleInterferenceGraph :: (MachineInstr i, Ord i, Show i)
         => (Map (Unique i) ActivityStorage, Set (Unique i))
         -> DirectedGraph Temp
    singleInterferenceGraph (activity, instructions) = 
        fst $ foldl' addTempNodes (emptyGraph, activity) (Set.toList instructions)

    addTempNodes :: (MachineInstr i, Ord i, Show i)
         => (DirectedGraph Temp, Map (Unique i) ActivityStorage)
         -> (Unique i)
         -> (DirectedGraph Temp, Map (Unique i) ActivityStorage) 
    addTempNodes (graph, activity) i =

        case (isMoveBetweenTemps . snd) i of
            Nothing -> do
                let defs = Set.toList $ (def . snd) i
                    outs = Set.toList $ out_a $ activity Map.! i
                    edges = concatMap (\d -> zip (repeat d) (filter (/= d) outs)) defs

                    g'   = foldl' addNode graph (defs ++ outs)
                    g''  = foldl' addBothEdges g' edges
                (g'', activity)
            (Just (t, v))  -> do

                  let outs :: [Temp]
                      outs = Set.toList $ out_a $ activity Map.! i

                  case (t `elem` outs) of
                        True -> do
                            let (edgedGraph, _, _) = foldl' go (graph, v, t) outs
                                nodeGraph = foldl' addNode edgedGraph (t : v : outs)
                            (nodeGraph, activity)
                        False -> (graph, activity)
        where
                go :: (DirectedGraph Temp, Temp, Temp) -> Temp -> (DirectedGraph Temp, Temp, Temp)
                go (g, v, t) u
                    | u /= v    = (addBothEdges g (t,u), v, t)
                    | otherwise = (g, v, t)

-- | we need to add both edges, as we use a directed graph data type for an undirected graph
addBothEdges :: DirectedGraph Temp -> (Temp, Temp) -> DirectedGraph Temp
addBothEdges g (a, b) = addEdge (addEdge g a b) b a




