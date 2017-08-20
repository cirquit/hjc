{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Cmm.X86.Spilling where

import           Cmm.LabelGenerator
import           Cmm.Backend         (MachineInstr(..), MachineFunction(..), MachinePrg(..))
import           Cmm.X86.InstrCore
import           Data.Int
import           Text.Printf
import           Data.Maybe          (fromJust)
import           Data.Map            (Map)
import qualified Data.Map as Map
import           Data.Set            (Set) 
import qualified Data.Set as Set
import           Data.List           (foldl')
import           Debug.Trace         (trace)

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens                       hiding ((#), use)
import           Control.Monad                      (when, zipWithM_, foldM)


-- | Instance definition for X86 Backend

-- |                   p       f        i
instance MachinePrg X86Prog X86Func X86Instr  where

--  machinePrgFunctions :: p -> [f]
    machinePrgFunctions p = x86functions p

--  replaceFunctions :: p -> [f] -> p
    replaceFunctions p fs = X86Prog fs


-- |                        f       i
instance MachineFunction X86Func X86Instr where

--  machineFunctionName  :: f -> String
    machineFunctionName f = x86functionName f

--  machineFunctionBody  :: f -> [i]
    machineFunctionBody f = x86body f

--  machineFunctionRename :: f -> (Temp -> Temp) -> f
    machineFunctionRename f replaceTemp = f { x86body = map (flip renameInstr replaceTemp) (x86body f) }

--  machineFunctionFilterInstructions :: f -> f
-- | we filter every instruction with a 'Temp', and 'MOVES' between same Temps
    machineFunctionFilterInstructions f = do
        let code     = zip (x86body f) (x86comments f)

            newCode = filter (not . badInstructions) code 

            (newBody, newComments) = unzip newCode

        f { x86body = newBody, x86comments = newComments }

--  machineFunctionRenameByMap :: f -> Map Temp Temp -> f
    machineFunctionRenameByMap f tempMapping = do
        let body = x86body f

            createMappings :: [(Temp -> Temp)]
            createMappings = map (\(k, v) -> renameTemp k v) $ Map.toAscList tempMapping
            -- TODO
            -- !z = trace ("Tempmappings: \n" ++ concatMap (\x -> show x ++ "\n") (Map.toAscList tempMapping)) 1

            newBody :: [X86Instr]
            newBody = map (\i -> foldl' renameInstr i createMappings) body

        f { x86body = newBody }

--  machineFunctionSpill :: MonadNameGen m => f -> Set Temp -> m f
    machineFunctionSpill f st = do
        state <- execStateT (spillTemps f) defaultSpilledState
        return $ f { x86body         = reverse $ view body     state
                   , x86comments     = reverse $ view comments state
                   , x86spilledCount = view padding state }
      where
          defaultSpilledState = SpillState
            {   _padding      = x86spilledCount f  -- current amount of elements on stack
              , _body         = []
              , _comments     = []
              , _spilledTemps = st
              , _tempPaddings = Map.empty
              , _tempMappings = Map.empty
            }



spillTemps :: MonadNameGen m => X86Func -> Spill m ()
spillTemps f = go (x86body f) (x86comments f)
    where
        go :: MonadNameGen m => [X86Instr] -> [X86Comment] -> Spill m ()
        go []     []     = return ()
        go (i:is) (c:cs) = do
            st <- view spilledTemps <$> get
            let used :: Set Temp
                used = Set.intersection (use i) st

                defd :: Set Temp
                defd = Set.intersection (def i) st

            usedInstructions   <- mapM genUsedInstructions $ Set.toAscList used
            defdInstructions   <- mapM genDefsInstructions $ Set.toAscList defd
            renamedInstruction <- replaceTemps i (used `Set.union` defd)

            mapM_ addInstructon usedInstructions
            addInstructon renamedInstruction
            mapM_ addInstructon defdInstructions
            go is cs


genUsedInstructions :: MonadNameGen m => Temp -> Spill m X86Instr
genUsedInstructions t = do
    newTemp <- getNewTemp t
    mpad <- Map.lookup t <$> (view tempPaddings <$> get)
    case mpad of
        Nothing    -> error $ "Cmm.I386Instr.checkUsedTemps: Temporary " ++ show t ++ " was used, but never initialized with a memory address"
        (Just pad) -> do
            let op = calculateMemoryAddress pad
            return $ move (Reg newTemp) op

genDefsInstructions :: MonadNameGen m => Temp -> Spill m X86Instr
genDefsInstructions t = do
    newTemp <- getRenamedTemp t
    op      <- getMemoryOperand t
    return $ move op (Reg newTemp)

getRenamedTemp :: MonadNameGen m => Temp -> Spill m Temp
getRenamedTemp t = do
    mtemp <- Map.lookup t <$> (view tempMappings <$> get)
    case mtemp of
        Nothing        -> getNewTemp t
        (Just newTemp) -> return newTemp

getNewTemp :: MonadNameGen m => Temp -> Spill m Temp
getNewTemp t = do
    newTemp <- lift $ nextTemp'
    tempMappings %= Map.insert t newTemp
    return newTemp

-- | if a padding is non-existent, the temorary was never 'defined' before and has to be
--   given a new address
getMemoryOperand :: MonadNameGen m => Temp -> Spill m Operand
getMemoryOperand t = do
    mpad <- Map.lookup t <$> (view tempPaddings <$> get)
    case mpad of
        Nothing -> do
            pad <- view padding <$> get
            tempPaddings %= Map.insert t pad
            padding += 1
            return $ calculateMemoryAddress pad
        (Just pad) -> return $ calculateMemoryAddress pad


move :: Operand -> Operand -> X86Instr
move op1 op2 = Binary MOV (sizeDirective, op1) (sizeDirective, op2)
  where
      sizeDirective = Nothing

calculateMemoryAddress :: Int32 -> Operand
calculateMemoryAddress i =
    let address = (-1) * 4 * (1 + i)
    in  Mem $
        EffectiveAddress
        { base         = Just ebpT
        , indexScale   = Nothing
        , displacement = address
        }

addInstructon :: MonadNameGen m => X86Instr -> Spill m ()
addInstructon i = appendInstrComm (i, emptyComment)

appendInstrComm :: MonadNameGen m => (X86Instr, X86Comment) -> Spill m ()
appendInstrComm (i, c) = do
    body     %= \l -> (i:l)
    comments %= \l -> (c:l)

-- | rename every spilled temp and reset the tempmappings, so the next time same temps are encountered,
--   a new temp will be generated
replaceTemps ::  MonadNameGen m => X86Instr -> Set Temp -> Spill m X86Instr
replaceTemps i toReplace = do
    newInstruction <- foldM go i toReplace
    tempMappings .= Map.empty
    return newInstruction
  where
      go :: MonadNameGen m => X86Instr -> Temp -> Spill m X86Instr
      go instr t = do
        tm <- view tempMappings <$> get
        case Map.lookup t tm of
            (Just newTemp) -> return $ renameInstr instr (renameTemp t newTemp) 
            Nothing        -> return instr

data SpillState = SpillState {
       _padding      :: Int32
     , _body         :: [X86Instr]
     , _comments     :: [X86Comment]
     , _spilledTemps :: Set Temp
     , _tempPaddings :: Map Temp Int32  -- which temp is already used and has a memory adress
     , _tempMappings :: Map Temp Temp   -- which temp is already renamed
   } deriving (Show, Eq, Ord)


type Spill m = StateT SpillState m

padding   :: Lens' SpillState Int32
padding = lens _padding (\x y -> x { _padding = y })

body   :: Lens' SpillState [X86Instr]
body = lens _body (\x y -> x { _body = y })

comments   :: Lens' SpillState [X86Comment]
comments = lens _comments (\x y -> x { _comments = y })

spilledTemps   :: Lens' SpillState (Set Temp)
spilledTemps = lens _spilledTemps (\x y -> x { _spilledTemps = y })

tempPaddings   :: Lens' SpillState (Map Temp Int32)
tempPaddings = lens _tempPaddings (\x y -> x { _tempPaddings = y })

tempMappings   :: Lens' SpillState (Map Temp Temp)
tempMappings = lens _tempMappings (\x y -> x { _tempMappings = y })


renameTemp :: Temp -> Temp -> Temp -> Temp
renameTemp toReplace replacement input
  | toReplace == input = replacement
  | otherwise          = input


-- |                     i
instance MachineInstr X86Instr where

--  use  :: i -> Set Temp
    use i                = Set.fromAscList $ x86Use i

--  def  :: i -> Set Temp
    def i                = Set.fromAscList $ x86Def i

--  isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
    isMoveBetweenTemps i = x86MovT i

--  isAssignmentToTemp :: i -> Maybe Temp
    isAssignmentToTemp i = x86AssignT i

--  jumps :: i -> [Label]
    jumps i              = x86Jump i

--  isFallThrough :: i -> Bool
    isFallThrough i      = x86FallThrough i

--  isLabel :: i -> Maybe Label
    isLabel i            = x86Label i

--  renameInstr :: i -> (Temp -> Temp) -> i
    renameInstr i f      = x86Rename i f

--  ret :: i
    ret = RET

addTemp :: Operand -> [Temp]
addTemp (Reg t)
  | espT == t = []
  | ebpT == t = []
  | otherwise = [t]
addTemp (Mem ea)
  | espT == t = []
  | ebpT == t = []
  | otherwise = [t]
  where
    t = fromJust $ base ea
addTemp _        = []

x86Use :: X86Instr -> [Temp]
x86Use (Unary  POP  (_, _))  = []
x86Use (Unary  PUSH (_, op)) = addTemp op
x86Use (Unary  IDIV (_, op)) = addTemp op
x86Use x@(Unary  _  (_, _))  = error $ "x86Use: didnt define rules for " ++ show x

x86Use x@(Binary MOV  (_, _) (_, op)) = addTemp op -- trace ("Use Mov for: " ++ show x ++ ", with temp " ++ show (addTemp op)) (addTemp op)
x86Use (Binary LEA  (_, _) (_, op)) = addTemp op

x86Use (Binary ADD  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary SUB  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary AND  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary OR   (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary XOR  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary CMP  (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use (Binary IMUL (_, op1) (_, op2)) = addTemp op1 ++ addTemp op2
x86Use x@(Binary _  (_, _)   (_, _))   = error $ "x86Use: didnt define rules for " ++ show x

x86Use  RET                      = [eaxT]
x86Use x@_                       = [] -- trace ("No temps for this guy: " ++ show x) []


x86Def :: X86Instr -> [Temp]
x86Def (Unary  POP  (_, op)) = addTemp op
x86Def (Unary  PUSH (_, _))  = []
x86Def (Unary  IDIV (_, _))  = [edxT, eaxT]
x86Def x@(Unary _   (_, _))  = error $ "x86Def: didnt define rules for " ++ show x

x86Def x@(Binary MOV    (_, op) (_, _)) = addTemp op -- trace ("Def Mov for: " ++ show x ++ ", with temp " ++ show (addTemp op)) (addTemp op)
x86Def (Binary LEA    (_, op) (_, _)) = addTemp op
x86Def (Binary ADD    (_, op) (_, _)) = addTemp op
x86Def (Binary SUB    (_, op) (_, _)) = addTemp op
x86Def (Binary AND    (_, op) (_, _)) = addTemp op
x86Def (Binary OR     (_, op) (_, _)) = addTemp op
x86Def (Binary XOR    (_, op) (_, _)) = addTemp op
x86Def (Binary CMP    (_, _)  (_, _)) = []
x86Def (Binary IMUL   (_, op) (_, _)) = addTemp op
x86Def x@(Binary _    (_, op) (_, _)) = error $ "x86Def: didnt define rules for " ++ show x

x86Def (CALL _) = [eaxT]
x86Def x@_                         = [] -- trace ("No temps for this guy: " ++ show x) []


x86MovT :: X86Instr -> Maybe (Temp, Temp)
x86MovT (Binary MOV (_, Reg t1) (_, Reg t2))
  | (t1 == ebpT) || (t1 == espT) || (t2 == ebpT) || (t2 == espT) = Nothing
  | otherwise                                                    = Just (t1, t2)
x86MovT _                                                        = Nothing

x86AssignT :: X86Instr -> Maybe Temp
x86AssignT (Unary _  (_, (Reg t))   )  = Just t
x86AssignT (Binary _ (_, (Reg t1)) _)  = Just t1
x86AssignT _                           = Nothing

x86Jump :: X86Instr -> [Label]
x86Jump (JMP l)  = [l]
x86Jump (J _ l)  = [l]
x86Jump _        = []

x86Label :: X86Instr -> Maybe Label
x86Label (LABEL l) = Just l
x86Label _         = Nothing

x86Rename :: X86Instr -> (Temp -> Temp) -> X86Instr
x86Rename (Unary i (s, Reg t)) f                 = Unary  i (s,  Reg (f t))
x86Rename (Unary i (s, Mem m)) f                 = Unary  i (s,  Mem (rMem f m))
x86Rename (Binary i (s1, Reg t1) (s2, Reg t2)) f = Binary i (s1, Reg (f t1)) (s2, Reg (f t2))
x86Rename (Binary i (s1, Mem m1) (s2, Mem m2)) f = Binary i (s1, Mem (rMem f m1)) (s2, Mem (rMem f m2))

x86Rename (Binary i op1@_        (s2, Reg t2)) f = Binary i op1 (s2, Reg (f t2))
x86Rename (Binary i op1@_        (s2, Mem m2)) f = Binary i op1 (s2, Mem (rMem f m2))
x86Rename (Binary i (s1, Reg t1) op2@_       ) f = Binary i (s1, Reg (f t1)) op2
x86Rename (Binary i (s1, Mem m1) op2@_       ) f = Binary i (s1, Mem (rMem f m1)) op2

x86Rename i _                                    = i

rMem :: (Temp -> Temp) -> EffectiveAddress -> EffectiveAddress
rMem f (EffectiveAddress (Just t1) is@_ d) = EffectiveAddress (Just (f t1)) is d

x86FallThrough :: X86Instr -> Bool
x86FallThrough (RET)   = False
x86FallThrough (JMP _) = False
x86FallThrough _       = True

usedTemps :: X86Instr -> [Temp]
usedTemps (Unary  _ (_, Reg t))              = [t]
usedTemps (Unary  _ (_, Mem m))              = getMemoryTemps m
usedTemps (Binary _ (_, Reg t1) (_, Reg t2)) = [t1,t2]
usedTemps (Binary _ (_, Mem m1) (_, Mem m2)) = getMemoryTemps m1 ++ getMemoryTemps m2
usedTemps (Binary _ _           (_, Reg t))  = [t]
usedTemps (Binary _ _           (_, Mem m))  = getMemoryTemps m
usedTemps (Binary _ (_, Reg t) _      )      = [t]
usedTemps (Binary _ (_, Mem m) _      )      = getMemoryTemps m
usedTemps i                                  = []

getMemoryTemps :: EffectiveAddress -> [Temp]
getMemoryTemps (EffectiveAddress (Just t1) _ _) = [t1]

sameBinaryMove :: X86Instr -> Bool
sameBinaryMove (Binary MOV (_,op1) (_,op2)) = op1 == op2
sameBinaryMove _ = False

invalidTemp :: X86Instr -> Bool
invalidTemp i = any areGeneratedTemps temps
    where temps = usedTemps i

areGeneratedTemps :: Temp -> Bool
areGeneratedTemps (Temp _) = True
areGeneratedTemps _        = False

badInstructions :: (X86Instr, X86Comment) -> Bool
badInstructions (i,_) = sameBinaryMove i || invalidTemp i

