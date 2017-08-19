{-# LANGUAGE BangPatterns #-}

module Cmm.ControlFlowGraph (
    createControlFlowGraph
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
import           Debug.Trace            (trace)


-- createControlFlowGraph :: (MachinePrg p f i, Ord i, Show i) => p -> [DirectedGraph i]
-- createControlFlowGraph p = map (addFunctionToGraph emptyGraph) (machinePrgFunctions p)

createControlFlowGraph :: (MachineFunction f i, Ord i, Show i) => f -> DirectedGraph i
createControlFlowGraph f = addInstructionsToGraph emptyGraph body body
    where
        body = machineFunctionBody f

-- | creates a directed graph of the instructions and by connecting
--   subsequent instructions and the labels if it is a jump

-- TODO refactor this
addInstructionsToGraph :: (MachineInstr i, Ord i, Show i) => DirectedGraph i -> [i] -> [i] -> DirectedGraph i
addInstructionsToGraph g []     imm = g
addInstructionsToGraph g (i:is) imm = do

    let g1 = addNode g i

    case (jumps i) of
        [l] -> do
            let (Just labelInstr) = find (\x -> (maybe "" id (isLabel x)) == l ) imm
                g2 = addEdge g1 i labelInstr
            case (take 1 is) of
                (j:_) -> do
                    let g3 = addEdge g2 i j
                    addInstructionsToGraph g3 is imm
                []  -> do
                    addInstructionsToGraph g2 is imm

        _   -> do
            case (take 1 is) of
                (j:_) -> do
                    let g2 = addEdge g1 i j 
                    addInstructionsToGraph g2 is imm
                []  -> do
                    addInstructionsToGraph g1 is imm