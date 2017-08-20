{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Cmm.Register.Allocation(
     allocateAllRegistersGen
   , generateAllocatedx86
    ) where 

import           Data.Set                   (Set)
import qualified Data.Set        as Set
import           Data.Map                   (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad.IO.Class
import           Control.Monad.Loops        (maximumOnM)
import           Control.Lens               hiding ((#), none)
import           Control.Monad.Trans.State  hiding (State)
import           Control.Monad.Trans        (lift)

import           Text.Printf                (printf)
import           Data.List                  (foldl', find, maximumBy)
import           Debug.Trace                (trace)
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (comparing)

import           AST                        (MiniJava())
import           Cmm.X86.Backend            (generatex86Gen)
import           Cmm.X86.InstrCore
import           Cmm.DirectedGraph
import           Cmm.Backend                (MachineInstr(..)
                                           , MachineFunction(..)
                                           , MachinePrg(..)
                                           , CodeGen(..))
import           Cmm.LabelGenerator     

import           Cmm.ControlFlowGraph       (createControlFlowGraph)
import           Cmm.ActivityAnalysis       (activityAnalysis, ActivityStorage(..))
import           Cmm.InterferenceGraph      (createInterferenceGraph)
import           Cmm.Register.Core      

generateAllocatedx86 :: MiniJava -> IO X86Prog
generateAllocatedx86 ast = runNameGenT $ generatex86Gen ast >>= go c
  where c = X86CodeGen

        go :: X86CodeGen -> X86Prog -> NameGenT IO X86Prog
        go c p = allocateAllRegistersGen c p


allocateAllRegistersGen :: (CodeGen c p f i, Ord i, Show i) => c -> p -> NameGenT IO p
allocateAllRegistersGen c prog = do
    functions <- mapM (allocateRegisters c) (machinePrgFunctions prog)
    return $ replaceFunctions prog functions

-- | approximates the graph coloring problem, spills the temps and "should" return a colored function function
--
allocateRegisters ::
  (CodeGen c p f i, Ord i, Show i)
  => c
  -> f
  -> NameGenT IO f
allocateRegisters c function = evalStateT (modifyFunction function) regState
    where
        ig :: DirectedGraph Temp
        ig = createInterferenceGraph function

        regState = RegisterState
            { _interferenceGraph = ig
            , _tempStates        = createDefaultTempStates ig
            , _colors            = generalPurposeRegisters c
            , _tempStack         = []
            }

modifyFunction :: (MonadNameGen m, MonadIO m, MachineFunction f i, Ord i, Show i) => f -> Reg m f
modifyFunction f = do
    coloringPass
    spilled <- getAllSpilled
    case (not . none $ spilled) of
        True -> do
            newFunction <- lift $ machineFunctionSpill f spilled  -- modifying the function with asm
            updateInterferenceGraph newFunction            -- create new interferencegraph
            deleteSpilledTemps spilled                     -- delete spilled from our local (temp -> state mapping)
            resetAllSpilledTemps                           -- if you didn't spill every possible temp, this would reset them accordignly
            addNewTempStates                               -- new temps are generated by spilling, we need to add them
            modifyFunction newFunction
        False -> insertRegisterColors f
            
insertRegisterColors :: (MonadNameGen m, MonadIO m, MachineFunction f i, Ord i, Show i) => f -> Reg m f
insertRegisterColors f = machineFunctionRenameByMap f <$> createTempMapping

coloringPass :: (MonadNameGen m, MonadIO m) => Reg m ()
coloringPass = do
    simplify
    mt <- findCurrentMaxChildrenNode
    case mt of
        Nothing  -> select
        (Just t) -> moveToStack t >> coloringPass

simplify :: (MonadNameGen m, MonadIO m) => Reg m ()
simplify = do
    ns <- allNodes
    k  <- Set.size <$> (view colors <$> get)
    lessThanKNodes <- filterSM (((< k) <$>) . getOutDegree) ns
    mapSM_ moveToStack lessThanKNodes

-- | pick temps from the stack and give them a possible color,
--   if none are possible, mark as spilled
--   terminate when the stack is empty
select :: (MonadNameGen m, MonadIO m) => Reg m ()
select  = do
    mtemp <- getFromStack
    case mtemp of
        Nothing  -> return ()
        (Just temp) -> do
            children <- getChildren temp
            mColor   <- getFreeColor children
            case mColor of
                Nothing      -> setSpilled temp >> select
                (Just color) -> do
                    tempStates %= Map.insert temp color
                    select

-- | get node which is not on the stack, pick the one with the maximum children, otherwise there are not 'Clear' nodes
findCurrentMaxChildrenNode ::  (MonadNameGen m, MonadIO m) => Reg m (Maybe Temp)
findCurrentMaxChildrenNode = do
    allNodes           <- nodes <$> (view interferenceGraph <$> get)
    allAccessableNodes <- filterSM (\n -> not <$> (onStack n)) allNodes
    case (not . none $ allAccessableNodes) of
        True ->  maximumOnM getOutDegree (Set.toAscList allAccessableNodes)
        False -> return Nothing

-- | check which colors are used, pick one if possible
getFreeColor :: (MonadNameGen m, MonadIO m) => Set Temp -> Reg m (Maybe State)
getFreeColor children = do
    validChildren <- filterSM isColored children
    usedColors    <- mapSM getColor validChildren
    allColors     <- view colors <$> get         
    let possibleColors = Set.difference allColors usedColors
    case none possibleColors of
        True ->  return Nothing
        False -> return . Just . Colored $ 0 `Set.elemAt` possibleColors