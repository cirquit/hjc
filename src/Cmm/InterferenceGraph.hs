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

-- TODO
import           System.IO.Unsafe       (unsafePerformIO)

createInterferenceGraph ::
  (MachineFunction f i, Ord i, Show i)
  => f
  -> DirectedGraph Temp
createInterferenceGraph function = do
    let -- cfgraph :: DirectedGraph i
        cfgraph = createControlFlowGraph function
        -- activity :: Map i ActivityStorage
        activity = activityAnalysis cfgraph
        -- instructions :: Set i
        instructions = nodes cfgraph
       -- !z = trace (concatMap (\x -> show x ++ "\n") instructions) 1
        -- states :: (Map i ActivityStorage, i)
        states = (activity, instructions)

    singleInterferenceGraph states

  where

    singleInterferenceGraph :: (MachineInstr i, Ord i, Show i) => (Map i ActivityStorage, Set i) -> DirectedGraph Temp
    singleInterferenceGraph (activity, instructions) = 
        fst $ foldl' addTempNodes (emptyGraph, activity) (Set.toAscList instructions)  

    addTempNodes :: (MachineInstr i, Ord i, Show i) => (DirectedGraph Temp, Map i ActivityStorage) -> i -> (DirectedGraph Temp, Map i ActivityStorage) 
    addTempNodes (graph, activity) i =
        -- let  !a = trace ("Current instruction: " ++ show i) 1
        -- in  
        case isMoveBetweenTemps i of
            Nothing -> do
                let defs = Set.toAscList $ def i
                    outs = Set.toAscList $ out_a $ activity Map.! i
                    edges = concatMap (\d -> zip (repeat d) (filter (/= d) outs)) defs

                    -- !b = trace ("Edges: " ++ show edges) 1 
    
                    g'   = foldl' addNode graph (defs ++ outs)
                    g''  = foldl' (\g (x,y) -> addEdge g x y) g' edges
                (g'', activity)
            (Just (dst, src))  -> do
                let outs  = filter (`notElem` [dst,src]) $ Set.toAscList $ out_a $ activity Map.! i
                    edges = zip (repeat dst) outs
                    -- !b = trace ("Edges: " ++ show edges) 1 
                    g'    = foldl' (\g (x,y) -> addEdge g x y) graph edges
                    -- g''   = addNode g' src
                    -- g_''  = addNode g'' dst
                (g', activity)









