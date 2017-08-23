{-# LANGUAGE BangPatterns #-}

module Cmm.ActivityAnalysis
  ( ActivityStorage(..)
  , activityAnalysis
  ) where

import Cmm.DirectedGraph
import Cmm.Backend
       (MachineInstr(..), MachineFunction(..), MachinePrg(..))
import Cmm.LabelGenerator (Temp())

import Cmm.ControlFlowGraph (createControlFlowGraph, Unique(..))

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Text.Printf (printf)
import Data.List (foldl', find)
import Data.Maybe (fromJust)

-- | simple data type to store the
--     written temps  ~ out
--     accessed temps ~ in
data ActivityStorage = ActivityStorage
    { out_a :: Set Temp
    , in_a :: Set Temp
    } deriving (Show, Eq, Ord)

emptyActivityStorage :: ActivityStorage
emptyActivityStorage =
    ActivityStorage
    { out_a = Set.empty
    , in_a = Set.empty
    }

-- | analyses the activity of the temporaries
--   by reversing the control flow graph, doing an almost correct
--   depth-first search (the successors are Sets)
--   and applying the proposed algorithm
--
activityAnalysis
    :: (MachineInstr i, Ord i, Show i)
    => DirectedGraph (Unique i) -> Map (Unique i) ActivityStorage
activityAnalysis graph =
    -- revGraph :: DirectedGraph (Int, i)
    let revGraph = reverseGraph graph

        lastReturn = (Set.size (nodes revGraph) + 1, ret)
        -- revNodes :: (Ord i) => [(Int, i)]
        revNodes = toList revGraph lastReturn
        -- livelinessMap :: Map (Int, i) ActivityStorage
        livelinessMap =
            Map.fromList $ zip revNodes (repeat emptyActivityStorage)
        -- runUpate :: Map (Int, i) ActivityStorage -> Map (Int, i) ActivityStorage
        runUpate lm = fst $ foldl' updateActivities (lm, graph) revNodes
        -- newMap :: Map (Int, i) ActivityStorage
        solvedMap = repeatUntilSame livelinessMap runUpate
    in solvedMap
  where
    updateActivities (lm, g) i =
            -- succs ::  [(Int, i)]
        let succs = Set.toList $ successors g i
            -- activity_ins :: [Set i]
            activitiy_ins = map (\i -> in_a $ fromJust $ Map.lookup i lm) succs
            -- out_ :: Set Temp
            outs = out_a $ fromJust $ Map.lookup i lm
            -- in_  :: Set Temp
            in_ = ((use . snd) i) `Set.union` (outs `Set.difference` ((def . snd) i))

            out_ = (Set.unions $ activitiy_ins)
            activitiy =
                ActivityStorage
                { out_a = out_
                , in_a = in_
                }
        in (Map.insert i activitiy lm, g)

repeatUntilSame :: Eq s => s -> (s -> s) -> s
repeatUntilSame state transform = do
    let newState = transform state
    case state == newState of
        True -> state
        False -> repeatUntilSame newState transform
