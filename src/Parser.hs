module Parser where

import BNF
import qualified Data.Map as Map
import Control.Monad (foldM)
-- import Control.Monad.State

type RAM = Map.Map String Integer

eval :: StmList -> IO ([Integer], RAM)
eval stmList = evalStmList ([], Map.empty) stmList

evalStmList :: ([Integer], RAM) -> StmList -> IO ([Integer], RAM)
evalStmList (nums,ram) (StmList list) = foldM evalStm (nums, ram) list

evalStm :: ([Integer], RAM) -> Stm -> IO ([Integer], RAM)
evalStm (nums, ram) (PrintStm expList) = do
    (nums1, _) <- evalExpList (nums, ram) expList
    print $ concatMap (\x -> show x ++ " ") nums1
    return ([], ram)
evalStm (nums, ram) (AssignStm id exp) = do
    (nums1, ram1) <- evalExp (nums, ram) exp
    let deci = last nums1
    let nums1 = init nums1
    let ram2  = Map.insert id deci ram1
    return (nums1, ram2)

evalExpList :: ([Integer], RAM) -> ExpList -> IO ([Integer], RAM)
evalExpList (nums,ram) (ExpList expList) = foldM evalExp (nums, ram) expList

evalExp :: ([Integer], RAM) -> Exp -> IO ([Integer], RAM)
evalExp (nums, ram) (Exp primExp rest) = do
    (nums1, ram1) <- evalPrimExp ram primExp
    let deci = last nums1
    (nums2, ram2) <- evalBinOpPrimExpList ram1 deci rest
    return (nums ++ nums1 ++ nums2, ram2)

evalBinOpPrimExpList :: RAM -> Integer -> [(BinOp, PrimExp)] -> IO ([Integer], RAM)
evalBinOpPrimExpList ram deci [] = return ([deci], ram)
evalBinOpPrimExpList ram deci ((op, pexp):xs) = do
   (nums1, ram1) <- evalPrimExp ram pexp
   let deci1 = last nums1
   let f = evalBinOp op
   evalBinOpPrimExpList ram1 (deci `f` deci1) xs 


evalPrimExp :: RAM -> PrimExp ->  IO ([Integer], RAM)
evalPrimExp ram (IdExp id)
    | (Just num) <- Map.lookup id ram = return ([num], ram)
    | otherwise                       = error $ "hjc: variable " ++ id ++ " not defined, but used"
evalPrimExp ram (NumExp num) = return ([num], ram)
evalPrimExp ram (EseqExp stmList exp) = do
    (nums, ram1)  <- evalStmList ([], ram) stmList
    (nums1, ram2) <- evalExp (nums, ram1) exp
    return (nums1, ram2)

-- evalPrimExp :: RAM -> PrimExp -> RAM
-- evalPrimExp ram (IdExp id) = undefined

evalBinOp :: BinOp -> (Integer -> Integer -> Integer)
evalBinOp PLUS  = (+)
evalBinOp MINUS = (-)
evalBinOp TIMES = (*)
evalBinOp DIV   = div