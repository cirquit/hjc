module Cmm.X86.ControlFlowGraph where 

import Cmm.DirectedGraph
import Cmm.Backend          (MachineInstr(..), MachineFunction(..), MachinePrg(..))
-- import Cmm.I386Instr

import Data.List         (foldl')

createDirectedGraph :: (MachinePrg p f i, Ord i) => p -> [DirectedGraph i]
createDirectedGraph p = map (addFunctionToGraph emptyGraph) (machinePrgFunctions p)

addFunctionToGraph :: (MachineFunction f i, Ord i) => DirectedGraph i -> f -> DirectedGraph i
addFunctionToGraph g f = addInstructionsToGraph g (machineFunctionBody f)

addInstructionsToGraph :: (MachineInstr i, Ord i) => DirectedGraph i -> [i] -> DirectedGraph i
addInstructionsToGraph g []     = g
addInstructionsToGraph g (i:is) = do
    let g1 = addNode g i 
        g2 = foldl' (\gr -> addEdge gr i) g1 (take 1 is)
    addInstructionsToGraph g2 is

-- | A node will be created if a temporary is defined
-- addIfDefined :: (MachineInstr i, Ord i) => DirectedGraph i -> i -> DirectedGraph i
-- addIfDefined g i =
--     case def i of
--         [t] -> addNode 
--         _   -> g