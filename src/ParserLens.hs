module ParserLens where

import BNF
import qualified Data.Map as Map
import Control.Monad (foldM)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Lens

-- boilerplate
io :: MonadIO m => IO a -> m a 
io = liftIO

type RAM = Map.Map String Integer

data SLMemory = SLMemory
     { _printValues :: [Integer]     -- values we want to print if the current expression is evaluated
     , _returnValue :: Maybe Integer -- value we want to return for a binary expression
     , _memory      :: RAM           -- lookup table for named values
     , _errors      :: [String]      -- possible errors while parsing (not used now)
     }
    deriving (Show, Eq)

printValues :: Lens' SLMemory [Integer]
printValues = lens _printValues (\x y -> x { _printValues = y })

returnValue :: Lens' SLMemory (Maybe Integer)
returnValue = lens _returnValue (\x y -> x { _returnValue = y })

memory :: Lens' SLMemory RAM
memory = lens _memory (\x y -> x { _memory = y })

errors :: Lens' SLMemory [String]
errors = lens _errors (\x y -> x { _errors = y })


-- | starting state for every straightline program
-- 
startMemory :: SLMemory
startMemory = SLMemory
    { _printValues = []
    , _returnValue = Nothing
    , _memory      = Map.empty
    , _errors      = []
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
    values <- concatMap (\v -> show v ++ " ") . view printValues <$> get
    io $ putStrLn values
    put slm    -- restore previous state
evalStm (AssignStm id exp) = do
    evalExp exp 
    mnum <- view returnValue <$> get
    case mnum of
        (Just num) -> memory %= Map.insert id num
        Nothing    -> error $ "hjc: variable " ++ id ++ " does not have an assignable value"
 

evalPrintExpList :: ExpList -> StateT SLMemory IO ()
evalPrintExpList (ExpList []) = return ()
evalPrintExpList (ExpList (exp:exps)) = do
    evalExp exp
    mnum <- view returnValue <$> get
    case mnum of
        Nothing    -> error $ "hjc: print function can't show this value"
        (Just num) -> do
            printValues %= (++ [num])
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
    mnum <- Map.lookup id . view memory <$> get
    case mnum of
        (Just num) -> returnValue .= Just num
        Nothing    -> error $ "hjc: variable " ++ id ++ " not defined, but used"
evalPrimExp (NumExp num) = returnValue .= Just num
evalPrimExp (EseqExp stmList exp) = do
    slm <- get -- save previous state
    evalStmList stmList
    put slm    -- restore previous state
    evalExp exp

evalBPList :: [(BinOp, PrimExp)] -> StateT SLMemory IO ()
evalBPList []                        = return ()
evalBPList ((binOp, primExp):bpList) = do
    let f = evalBinOp binOp
    mnum1 <- view returnValue <$> get
    evalPrimExp primExp
    mnum2 <- view returnValue <$> get
    case (mnum1, mnum2) of
        (Just num1, Just num2) -> do
            returnValue .= Just (num1 `f` num2)
            evalBPList bpList
        (Nothing, _) -> error $ "hjc: first argument of " ++ (show binOp) ++ " is not valid"
        (_, Nothing) -> error $ "hjc: second argument of " ++ (show binOp) ++ " is not valid"

evalBinOp :: BinOp -> (Integer -> Integer -> Integer)
evalBinOp PLUS  = (+)
evalBinOp MINUS = (-)
evalBinOp TIMES = (*)
evalBinOp DIV   = div