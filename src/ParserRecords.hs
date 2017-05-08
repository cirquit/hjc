module ParserRecords where

import BNF
import qualified Data.Map as Map
import Control.Monad (foldM)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- boilerplate
io :: MonadIO m => IO a -> m a 
io = liftIO

type RAM = Map.Map String Integer

data SLMemory = SLMemory
     { printValues :: [Integer]     -- values we want to print if the current expression is evaluated
     , returnValue :: Maybe Integer -- value we want to return for a binary expression
     , memory      :: RAM           -- lookup table for named values
     , errors      :: [String]      -- possible errors while parsing (not used now)
     }
    deriving (Show, Eq)

-- | starting state for every straightline program
-- 
startMemory :: SLMemory
startMemory = SLMemory
    { printValues = []
    , returnValue = Nothing
    , memory      = Map.empty
    , errors      = []
    }

-- | main evaluation function
--
eval :: StmList -> IO ((), SLMemory) 
eval ast = runStateT (evalStmList ast) startMemory

evalStmList :: StmList -> StateT SLMemory IO ()
evalStmList (StmList [])         = return ()
evalStmList (StmList (stm:stms)) = do
    evalStm stm
    evalStmList (StmList stms)

evalStm :: Stm -> StateT SLMemory IO ()
evalStm (PrintStm expList) = do
    slm <- get -- save previous state
    evalPrintExpList expList
    numbers <- printValues <$> get
    io . putStrLn $ concatMap (\n -> show n ++ " ") numbers
    put slm    -- restore previous state
evalStm (AssignStm id exp) = do
    evalExp exp
    mvalue <- returnValue <$> get
    case mvalue of
        (Just number) -> modify' $ \stm ->
            let memory' =  Map.insert id number (memory stm) in
            stm { memory = memory' }
        Nothing       -> error $ "hjc: variable " ++ id ++ " does not have an assignable value"

evalPrintExpList :: ExpList -> StateT SLMemory IO ()
evalPrintExpList (ExpList []) = return ()
evalPrintExpList (ExpList (exp:exps)) = do
    evalExp exp
    mnum <- returnValue <$> get
    case mnum of
        Nothing    -> error $ "hjc: print function can't show this value"
        (Just num) -> do
            modify' $ \stm -> 
                let pvalues = printValues stm in
                stm { printValues = pvalues ++ [num] }
            evalPrintExpList (ExpList exps)

evalExpList :: ExpList -> StateT SLMemory IO ()
evalExpList (ExpList [])         = return ()
evalExpList (ExpList (exp:exps)) = do
    evalExp exp
    evalExpList (ExpList exps)

evalExp :: Exp -> StateT SLMemory IO ()
evalExp (Exp primExp bpList) = do
    evalPrimExp primExp
    evalBPList bpList

evalPrimExp :: PrimExp -> StateT SLMemory IO ()
evalPrimExp (IdExp id) = do
    memory' <- memory <$> get
    case Map.lookup id memory' of
        (Just number) -> modify' (\stm -> stm { returnValue = (Just number) })
        Nothing       -> error $ "hjc: variable " ++ id ++ " not defined, but used"
evalPrimExp (NumExp number) = modify' (\stm -> stm { returnValue = (Just number) })
evalPrimExp (EseqExp stmList exp) = do
    slm <- get -- save previous state
    evalStmList stmList
    put slm    -- restore previous state
    evalExp exp

evalBPList :: [(BinOp, PrimExp)] -> StateT SLMemory IO ()
evalBPList []                        = return ()
evalBPList ((binOp, primExp):bpList) = do
    let f = evalBinOp binOp
    mnum1 <- returnValue <$> get
    evalPrimExp primExp
    mnum2 <- returnValue <$> get
    case (mnum1, mnum2) of
        (Just num1, Just num2) -> do
            modify' (\stm -> stm { returnValue = Just (num1 `f` num2) })
            evalBPList bpList
        (Nothing, _) -> error $ "hjc: first argument of " ++ (show binOp) ++ " is not valid"
        (_, Nothing) -> error $ "hjc: second argument of " ++ (show binOp) ++ " is not valid"

evalBinOp :: BinOp -> (Integer -> Integer -> Integer)
evalBinOp PLUS  = (+)
evalBinOp MINUS = (-)
evalBinOp TIMES = (*)
evalBinOp DIV   = div

