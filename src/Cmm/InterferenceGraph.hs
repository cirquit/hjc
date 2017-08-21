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

-- TODO
import           System.IO.Unsafe       (unsafePerformIO)

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

       -- !z = trace (concatMap (\x -> show x ++ "\n") instructions) 1
    singleInterferenceGraph states

  where

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
        -- let  !a = trace ("Current instruction: " ++ show i) 1
        -- in  
        case (isMoveBetweenTemps . snd) i of
            Nothing -> do
                let defs = Set.toList $ (def . snd) i
                    outs = Set.toList $ out_a $ activity Map.! i
                    edges = concatMap (\d -> zip (repeat d) (filter (/= d) outs)) defs

                    -- !b = trace ("1. Edges: " ++ show edges) 1 
                    g'   = foldl' addNode graph (defs ++ outs)
                    g''  = foldl' addBothEdges g' edges
                (g'', activity)
            (Just (dst, src))  -> do
                let outs  = filter (`notElem` [dst,src]) $ Set.toList $ out_a $ activity Map.! i
                    edges = zip (repeat dst) outs
          --          !b = trace ("2. Edges: " ++ show edges) 1 
                    g'    = foldl' addBothEdges graph edges
                    -- g''   = addNode g' src
                    g''  = addNode g' dst
                (g'', activity)

addBothEdges :: DirectedGraph Temp -> (Temp, Temp) -> DirectedGraph Temp
addBothEdges g (a, b) = addEdge (addEdge g a b) b a




