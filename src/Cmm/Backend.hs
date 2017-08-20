{-# LANGUAGE FunctionalDependencies #-}

module Cmm.Backend where

import Cmm.CAST (Cmm)
import Cmm.LabelGenerator (Temp, Label, MonadNameGen)
import Control.Monad.IO.Class
import Data.Set (Set)
import Data.Map (Map)

class MachineInstr i  where
    use :: i -> Set Temp
    def :: i -> Set Temp
    jumps :: i -> [Label]
    isFallThrough :: i -> Bool
    isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
    isAssignmentToTemp :: i -> Maybe Temp
    isLabel :: i -> Maybe Label
    renameInstr :: i -> (Temp -> Temp) -> i
    ret :: i

class (MachineInstr i) =>
      MachineFunction f i  | f -> i where
    machineFunctionName :: f -> String
    machineFunctionBody :: f -> [i]
    machineFunctionRename :: f -> (Temp -> Temp) -> f
    machineFunctionStackAlloc :: f -> f
    machineFunctionRenameByMap :: f -> Map Temp Temp -> f
    machineFunctionFilterInstructions :: f -> f
    machineFunctionSpill
        :: MonadNameGen m
        => f -> Set Temp -> m f

class (MachineInstr i, MachineFunction f i, Show p) =>
      MachinePrg p f i  | p -> f i where
    machinePrgFunctions :: p -> [f]
    replaceFunctions :: p -> [f] -> p

class (MachineInstr i, MachineFunction f i, MachinePrg p f i) =>
      CodeGen c p f i  | c -> p f i where
    codeGen
        :: (MonadNameGen m, MonadIO m)
        => c -> Cmm -> m p
    allRegisters :: c -> Set Temp
    generalPurposeRegisters :: c -> Set Temp
