{-# LANGUAGE BangPatterns #-}

module Cmm.ActivityAnalysis(
    ActivityStorage(..)
  , activityAnalysis) where 

import           Cmm.DirectedGraph
import           Cmm.Backend            (MachineInstr(..)
                                       , MachineFunction(..)
                                       , MachinePrg(..))
import           Cmm.LabelGenerator     (Temp())

import           Cmm.ControlFlowGraph   (createControlFlowGraph)

import           Data.Set               (Set)
import qualified Data.Set        as Set
import           Data.Map               (Map)
import qualified Data.Map.Strict as Map

import           Text.Printf            (printf)
import           Data.List              (foldl', find)
import           Debug.Trace            (trace)
import           Data.Maybe             (fromJust)

-- | simple data type to store the
--     written temps  ~ out
--     accessed temps ~ in
data ActivityStorage = ActivityStorage { out_a :: Set Temp, in_a :: Set Temp }
    deriving (Show, Eq, Ord)

emptyActivityStorage :: ActivityStorage
emptyActivityStorage = ActivityStorage { out_a = Set.empty, in_a = Set.empty }

-- | analyses the activity of the temporaries
--   by reversing the control flow graph, doing an almost correct
--   depth-first search (the successors are Sets)
--   and applying the proposed algorithm
--
activityAnalysis :: (MachineInstr i, Ord i, Show i) => DirectedGraph i -> Map i ActivityStorage
activityAnalysis graph = 
    let -- revNodes :: (Ord i) => [i]
        revNodes = toList (reverseGraph graph) ret

        -- livelinessMap :: Map i ActivityStorage
        livelinessMap = Map.fromList $ zip revNodes (repeat emptyActivityStorage)

        -- runUpate :: Map i ActivityStorage -> Map i ActivityStorage
        runUpate lm = fst $ foldl' updateActivities (lm, graph) revNodes
       
        -- newMap :: Map i ActivityStorage
        solvedMap  = repeatUntilSame livelinessMap runUpate

        -- debug - TODO
        -- !z         = trace (
        --     concatMap (\(instr, (ActivityStorage o i)) -> 
        --                     printf "%-30s out: %-20s in: %-20s \n"
        --                         (show instr)
        --                         (show (Set.toAscList o))
        --                         (show (Set.toAscList i))) (Map.toAscList solvedMap)) 1
    in solvedMap

  where
    -- updateActivities :: (Ord i, Show i) => (Map i ActivityStorage, DirectedGraph i) -> i -> (Map i ActivityStorage, DirectedGraph i)
    updateActivities (lm, graph) i =
        let -- succs ::  [i]
            succs     = Set.toAscList $ successors graph i
            -- activity_ins :: [Set i]
            activitiy_ins = map (\i -> in_a $ fromJust $ Map.lookup i lm) succs
            -- out_ :: Set Temp
            out_   = Set.unions $ activitiy_ins
            -- in_  :: Set Temp
            in_    = use i `Set.union` (out_ `Set.difference` def i)
            -- debug - TODO
            -- !z     = trace (
            --     unlines [
            --         "Instruction: " ++ show i
            --        ,"    succs:   " ++ show succs
            --        ,"    new_out: " ++ show out_
            --        ,"    new_in:  " ++ show in_
            --        ]) 1

            activitiy = ActivityStorage { out_a = out_, in_a = in_ }

        in (Map.insert i activitiy lm, graph)


repeatUntilSame :: Eq s => s -> (s -> s) -> s
repeatUntilSame state transform = do
    let newState = transform state
    case state == newState of
        True -> state
        False -> repeatUntilSame newState transform