{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Cmm.RegisterAllocation(
     allocateAllRegistersGen
   , generateAllocatedx86
    ) where 

import           Data.Set               (Set)
import qualified Data.Set        as Set
import           Data.Map               (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.IO.Class
import           Text.Printf            (printf)
import           Data.List              (foldl', find, maximumBy)
import           Debug.Trace            (trace)
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)

import           AST                    (MiniJava())
import           Cmm.X86.Backend        (generatex86Gen)
import           Cmm.I386Instr          -- (X86Prog(), X86CodeGen())
import           Cmm.Backend            
import           Cmm.DirectedGraph
import           Cmm.Backend            (MachineInstr(..)
                                       , MachineFunction(..)
                                       , MachinePrg(..))
import           Cmm.LabelGenerator     

import           Cmm.ControlFlowGraph   (createControlFlowGraph)
import           Cmm.ActivityAnalysis   (activityAnalysis, ActivityStorage(..))
import           Cmm.InterferenceGraph  (createInterferenceGraph)



generateAllocatedx86 :: MiniJava -> IO X86Prog
generateAllocatedx86 ast = runNameGenT $ generatex86Gen ast >>= go c
  where c = X86CodeGen
        
        go :: X86CodeGen -> X86Prog -> NameGenT IO X86Prog
        go c p = allocateAllRegistersGen c p


allocateAllRegistersGen :: (CodeGen c p f i, Ord i, Show i) => c -> p -> NameGenT IO p
allocateAllRegistersGen c prog = do
    functions <- mapM (allocateRegisters c) (machinePrgFunctions prog)
    liftIO $ putStrLn "HELLOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO!!!!!!!!!!!!!!!!!!"
    return $ replaceFunctions prog functions

-- | approximates the graph coloring problem, spills the temps and "should" return a coloringPassnable function
--T
allocateRegisters ::
  (CodeGen c p f i, Ord i, Show i)
  => c
  -> f
  -> NameGenT IO f
allocateRegisters c function = do
    let -- ig :: DirectedGraph Temp
        ig = createInterferenceGraph function
        -- ts :: Map Temp State
        ts = createDefaultTempStates ig
        -- io :: [Temp]  (insertion ordering) TODO
        io = []
        -- registers :: Set Temp
        registers = generalPurposeRegisters c

    go (ts, io) ig registers function

  where
      go :: (MachineFunction f i, Ord i, Show i) => (Map Temp State, [Temp]) -> DirectedGraph Temp -> Set Temp -> f -> NameGenT IO f
      go (ts, io) ig registers function = do
          let (ts', io')  = coloringPass (ts, io) ig registers
              spilled     = getAllSpilled ts'
          case (not . none $ spilled) of
              True -> do
                  function' <- machineFunctionSpill function spilled
                  let ig'   = createInterferenceGraph function'     -- new graph with newly inserted instructions
                      ts''  = deleteSpilledTemps ts spilled
                      ts_''  = resetAllSpilledTemps ts'              -- reset every spilled-marked node
                      ts__'' = addNewTempStates ts_'' ig'             -- we need to include newly inserted temps, but not clear the already colored ones
                  go (ts__'', io') ig' registers function'
              False -> return function

coloringPass :: (Map Temp State, [Temp]) -> DirectedGraph Temp -> Set Temp -> (Map Temp State, [Temp])
coloringPass (ts,io) ig colors = do
    let (ts',io') = simplify (ts,io) ig colors
        mt        = findCurrentMaxChildrenNode (ts', io') ig
    case mt of
        Nothing  -> select (ts', io') ig colors
        (Just t) -> do
            let (ts'', io'') = moveToStack (ts', io') t
            coloringPass (ts'', io'') ig colors


simplify :: (Map Temp State, [Temp]) -> DirectedGraph Temp  -> Set Temp -> (Map Temp State, [Temp])
simplify (ts,io) ig colors =
    let 
        -- ns :: Set Temp
        ns = nodes ig
        -- lessThanRegNodes :: Set Temp
        lessThanRegNodes = Set.filter (\n -> (getOutDegree (ts,io) ig n) < (Set.size colors)) ns
        -- stack :: [Temp]
        stack = reverse $ Set.toAscList lessThanRegNodes

    in foldl' moveToStack (ts,io) stack


findCurrentMaxChildrenNode :: (Map Temp State, [Temp]) -> DirectedGraph Temp -> Maybe Temp
findCurrentMaxChildrenNode (ts, io) ig = do
    let
        -- allAccessableNodes :: Set Temp
        allAccessableNodes = Set.filter (not . onStack (ts,io)) $ nodes ig

    if (not . null $ allAccessableNodes)
        then let
                -- maxChildrenNode :: Temp
                maxChildrenNode = maximumBy (comparing (getOutDegree (ts, io) ig)) allAccessableNodes
             in Just maxChildrenNode
        else Nothing

select :: (Map Temp State, [Temp]) -> DirectedGraph Temp -> Set Temp -> (Map Temp State, [Temp])
select (ts,io) ig colors = do
    let (mt, ts', io') = getFromStack (ts, io)
                      
    case mt of
        Nothing  -> (ts', io')
        (Just t) -> do
            let children = getChildren (ts', io') ig t
                possibleColor = getFreeColor (ts', io') children colors
                  
            case possibleColor of
                Nothing      -> do
                    let (ts'', io'') = setSpilled (ts', io') t
                    select (ts'', io'') ig colors
                   
                (Just color) -> do
                    let ts'' = Map.insert t color ts'
                    select (ts'', io') ig colors


getFreeColor :: (Map Temp State, [Temp]) -> Set Temp -> Set Temp -> Maybe State
getFreeColor (ts, io) children colors = do
    let 
        -- validChildren :: Set Temp
        validChildren = Set.filter (isColored (ts, io)) children
        -- usedColors :: Set Temp
        usedColors = Set.map (getColor (ts, io)) validChildren
        -- possibleColors :: Set Temp
        possibleColors = Set.difference colors usedColors

    case null possibleColors of
        True -> Nothing
        False -> Just $ Colored $ 0 `Set.elemAt` possibleColors

createDefaultTempStates :: DirectedGraph Temp -> Map Temp State
createDefaultTempStates ig = do
    let ns = Set.toAscList (nodes ig)
    foldl' (\g n -> Map.insert n defaultTempState g) Map.empty ns

-- | Temp state
--
data State =
      Colored Temp -- register name disguised as Temp
    | Spilled         
    | Clean
    | Stack
  deriving (Ord, Show, Eq)

defaultTempState :: State
defaultTempState = Clean

getChildren :: (Map Temp State, [Temp]) -> DirectedGraph Temp -> Temp -> Set Temp
getChildren (ts, io) ig t = do
    let children :: Set Temp
        children = successors ig t

    Set.filter (\n -> validChildren (ts Map.! n)) children

validChildren :: State -> Bool
validChildren Clean       = True
validChildren Stack       = False
validChildren (Colored _) = False
validChildren Spilled     = error "Cmm.RegisterAllocation.validChildren: There should never be any spilled nodes in the 'simplify' function"

getOutDegree :: (Map Temp State, [Temp]) -> DirectedGraph Temp -> Temp -> Int
getOutDegree (ts,io) ig t = Set.size $ getChildren (ts, io) ig t

setSpilled ::  (Map Temp State, [Temp]) -> Temp -> (Map Temp State, [Temp])
setSpilled (ts,io) t = (Map.insert t Spilled ts, io)

moveToStack :: (Map Temp State, [Temp]) -> Temp -> (Map Temp State, [Temp])
moveToStack (ts,io) n = (Map.insert n Stack ts, n : io)

onStack ::  (Map Temp State, [Temp]) -> Temp -> Bool
onStack (ts,io) t = (ts Map.! t) == Stack

getFromStack :: (Map Temp State, [Temp]) -> (Maybe Temp, Map Temp State, [Temp])
getFromStack  (ts, io) = 
  case take 1 io of
     [t] -> (Just t, Map.insert t Clean ts, drop 1 io)
     _   -> (Nothing, ts, io)

isColored :: (Map Temp State, [Temp]) -> Temp -> Bool
isColored (ts,io) t = 
  case (ts Map.! t) of
      (Colored _) -> True
      _           -> False

getColor :: (Map Temp State, [Temp]) -> Temp -> Temp
getColor (ts,io) t =
  case (ts Map.! t) of
      (Colored c) -> c
      s@_         -> error $ "Cmm.RegisterAllocation.getColor: temp " ++ show t ++ " does not have the state 'Colored', but: " ++ show s

getAllSpilled :: Map Temp State -> Set Temp
getAllSpilled = Set.fromAscList . map fst . filter (\(t, s) -> s == Spilled) . Map.toList

resetAllSpilledTemps :: Map Temp State -> Map Temp State
resetAllSpilledTemps ts = Map.map (\s -> if (s == Spilled) then Clean else s) ts

deleteSpilledTemps :: Map Temp State -> Set Temp -> Map Temp State
deleteSpilledTemps ts spilled = Set.foldl (\m e -> Map.delete e m) ts spilled

addNewTempStates :: Map Temp State -> DirectedGraph Temp -> Map Temp State
addNewTempStates ts ig = do
    let -- allNodes :: Set Temp
        allNodes = nodes ig 
        -- prevNodes :: Set Temp
        prevNodes = Map.keysSet ts
        -- newTemps :: Set Temp
        newTemps = Set.filter (`Set.notMember` prevNodes) allNodes 

    Set.foldl (\m e -> Map.insert e Clean m) ts newTemps

-- none :: [a] -> Bool
none = null