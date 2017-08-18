{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Cmm.InterferenceGraph(
    createInterferenceGraph
    ) where 

import           Cmm.DirectedGraph
import           Cmm.Backend            (MachineInstr(..)
                                       , MachineFunction(..)
                                       , MachinePrg(..))
import           Cmm.LabelGenerator     (Temp())

import           Cmm.ControlFlowGraph   (createControlFlowGraph)
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
  (MachinePrg p f i, Ord i, Show i)
  => p
  -> [DirectedGraph Temp]
createInterferenceGraph program = do
    let -- cfgraph :: [DirectedGraph i]
        cfgraph = createControlFlowGraph program
        -- activity :: [Map i ActivityStorage]
        activity = map activityAnalysis cfgraph
        -- instructions :: [Set i]
        instructions = map nodes cfgraph
        -- states :: (Map i ActivityStorage, i)
        states = zip activity instructions

    map singleInterferenceGraph states
  where

    singleInterferenceGraph :: (MachineInstr i, Ord i, Show i) => (Map i ActivityStorage, Set i) -> DirectedGraph Temp
    singleInterferenceGraph (activity, instructions) = 
        fst $ foldl' addTempNodes (emptyGraph, activity) (Set.toAscList instructions)  

    addTempNodes :: (MachineInstr i, Ord i, Show i) => (DirectedGraph Temp, Map i ActivityStorage) -> i -> (DirectedGraph Temp, Map i ActivityStorage) 
    addTempNodes (graph, activity) i =
        case isMoveBetweenTemps i of
            Nothing -> do
                let defs = Set.toAscList $ def i
                    outs = Set.toAscList $ out_a $ activity Map.! i
                    !a   = trace ("outs for " ++ show i ++ ": " ++ concatMap show outs) 1
                    edges = concatMap (\d -> zip (repeat d) outs) defs
                    g'   = foldl' addNode graph (defs ++ outs)
                    g''  = foldl' (\g (x,y) -> addEdge g x y) g' edges
                (g'', activity)
            (Just (_, src))  -> do
                let outs  = filter (/= src) $ Set.toAscList $ out_a $ activity Map.! i
                    edges = zip (repeat src) outs
                    g'    = foldl' (\g (x,y) -> addEdge g x y) graph edges
                (g', activity)