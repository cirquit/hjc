{-# LANGUAGE BangPatterns, PatternSynonyms, ExplicitNamespaces #-}

module Cmm.ControlFlowGraph (
    createControlFlowGraph
  , type Unique
    ) where 

import           Cmm.DirectedGraph
import           Cmm.Backend            (MachineInstr(..)
                                       , MachineFunction(..)
                                       , MachinePrg(..))
import           Cmm.LabelGenerator     (Temp())

import           Data.Set               (Set)
import qualified Data.Set        as Set
import           Data.Map               (Map)
import qualified Data.Map.Strict as Map

import           Text.Printf            (printf)
import           Data.List              (foldl', find)

type Unique i = (Int, i)

-- zipping instructions with their line number to created unique ids in order to allow same instructions to 
-- have different activities
createControlFlowGraph :: (MachineFunction f i, Ord i, Show i) => f -> DirectedGraph (Unique i)
createControlFlowGraph f = addInstructionsToGraph emptyGraph body body
    where
        body = zip [1..] (machineFunctionBody f)

-- | creates a directed graph of the instructions and by connecting
--   subsequent instructions and the labels if it is a jump

addInstructionsToGraph :: (MachineInstr i, Ord i, Show i) => DirectedGraph (Unique i) -> [(Unique i)] -> [(Unique i)] -> DirectedGraph (Unique i)
addInstructionsToGraph g []         imm = g
addInstructionsToGraph g ((n,i):is) imm = do

    let g1 = addNode g (n,i)

    case (jumps i) of
        [l] -> do
            let (Just (lineNum, labelInstr)) = find (\(_, x) -> (maybe "" id (isLabel x)) == l ) imm
                g2 = addEdge g1 (n,i) (lineNum, labelInstr)
            case ((isFallThrough i), (take 1 is)) of
                (True, ((m,j):_)) -> do
                    let g3 = addEdge g2 (n,i) (m,j)
                    addInstructionsToGraph g3 is imm
                (False, _)  -> do
                    addInstructionsToGraph g2 is imm

        _   -> do
            case ((isFallThrough i), take 1 is) of
                (True, ((m,j):_)) -> do
                    let g2 = addEdge g1 (n,i) (m,j) 
                    addInstructionsToGraph g2 is imm
                (False, _)  -> do
                    addInstructionsToGraph g1 is imm