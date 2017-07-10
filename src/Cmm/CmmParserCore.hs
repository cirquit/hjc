module Cmm.CmmParserCore where

import qualified Data.Map                   as Map
import           Data.Int
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
-- import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           AST
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.CAST
import qualified SymbolTable                as ST

-- | C-- Monad
--
--  Based on the NameGenT so we can use the `nextLabel` with internal state without contaminating the CmmScope
--  Outer Transformer is the StateT which holds the CmmScope
--
--
type CM m a = StateT CmmScope (NameGenT m) a

data CmmScope = CmmScope
    { _curClass         :: Maybe Class
    , _curMethod        :: Maybe Method
    , _localObjectType  :: Maybe Identifier
    , _symbols          :: ST.MiniJavaTable
    , _cmm              :: Cmm
    , _curRetTemp       :: CmmExp
    , _localTemps       :: Map.Map Identifier CmmExp  -- (CmmExp == TEMP, PARAM(i), MEM(PARAM + n*4))
    , _localVars        :: Map.Map Identifier Type    -- variable type mapping for naming of methods, not happy with two maps
    }


-- | defined in LabelGenerator.hs
--
-- | Generates a fresh temporary. The returned temporary is
--   guaranteed to be different from all the ones returned previously
--   and the ones give to 'avoid'.nextTemp :: Monad m => CM m Temp
nextTemp :: CM IO Temp
nextTemp = lift nextTemp'

nextTempE :: CM IO CmmExp
nextTempE = TEMP <$> lift nextTemp'

nextTempTE :: CM IO (Temp, CmmExp)
nextTempTE = nextTemp >>= \t -> return (t, TEMP t)

-- | Generates a fresh label.
nextLabel :: CM IO Label
nextLabel = lift nextLabel'

nextLabelLE :: CM IO (Label, CmmExp)
nextLabelLE = nextLabelLSE >>= \(l, _, le) -> pure (l, le)

nextLabelLS :: CM IO (Label, CmmStm)
nextLabelLS = nextLabelLSE >>= \(l, ls, _) -> pure (l,ls)

nextLabelLSE :: CM IO (Label, CmmStm, CmmExp)
nextLabelLSE = nextLabel >>= \l -> pure (l, LABEL l, NAME l)

-- | Declare that a list of temps must be avoided by 'nextTemp'
--  'nextTemp' will not return a temp that was passed to 'avoid'
avoid :: [Temp] -> CM IO ()
avoid = lift . avoid'

-- | we use MOVE(Temporary, exp) to evaluate the expression and never use the variable again
--
moveTemp :: CmmExp -> CM IO CmmStm
moveTemp e = MOVE <$> nextTempE <*> pure e

-- | we lookup how many member a class has
--   and multiply this by 4 (halloc takes bytes and everything is described through Int32)
--
--   always adding 1 (~ 8 byte) for the pointer to object
--
calculateClassMemoryCost :: Identifier -> CM IO Int32
calculateClassMemoryCost id = do
    sym <- view symbols <$> get
    let mcls = ST.lookupClassSymbols id sym
    case mcls of
        (Just cls) -> return . (4*) . (1+) $ ST.getMemberCount cls
        Nothing    -> error $ "CmmParser.hs:calculateClassMemoryCost - class \"" ++ id ++ "\" could not be found. Terminating."

-- | sizeExpression + 1 for the length member
--   times 4 because everything is in byte
--
calculateIntArrMemoryCost :: CmmExp -> CM IO CmmExp
calculateIntArrMemoryCost ce = do
    return $ BINOP MUL_C (BINOP PLUS_C  ce (CONST 1)) (CONST 4)

addZeroReturn :: CmmMethod -> CM IO CmmMethod
addZeroReturn m = do
    t <- nextTemp
    return $ m { cmmBody = cmmBody m ++ [MOVE (TEMP t) (CONST 0)], cmmReturn = t }

addArgumentsToScope :: CM IO ()
addArgumentsToScope = do
    (Just meth) <- view curMethod <$> get
    let argsNames = map _variableName (_methodArguments meth)
        argsTypes = map _type         (_methodArguments meth)
        argsExps  = map PARAM [1..]
    zipWithM_ (\name exp -> localTemps %= Map.insert name exp) argsNames argsExps
    zipWithM_ (\name typ -> localVars  %= Map.insert name typ) argsNames argsTypes

-- | our solution to 'run' a CmmExp
evalCExpE :: CmmExp -> CM IO CmmExp
evalCExpE cexp = flip ESEQ (CONST 1) <$> evalCExpS cexp

-- | our solution to 'run' a CmmExp
evalCExpS :: CmmExp -> CM IO CmmStm
evalCExpS cexp = MOVE <$> nextTempE <*> pure cexp


callAlloc :: CmmExp -> CM IO CmmExp
callAlloc ce = do
    let l = NAME $ mkLabel "_halloc"
    return $ CALL l [ce]

callRaise :: CmmExp -> CM IO CmmExp
callRaise ce = do
    let l = NAME $ mkLabel "_raise"
    return $ CALL l [ce]

indexOutOfBounds :: CM IO CmmExp
indexOutOfBounds = return $ CONST 111

getCallerType :: Identifier -> CM IO Type
getCallerType id = do
    lvars <- view localVars <$> get
    let (Just callerType) = Map.lookup id lvars
    return callerType


withClass :: Class -> CM IO a -> CM IO ()
withClass c f = do
    curClass  .= Just c
    localVars %= Map.insert "this" (IdT (_className c))
    f
    localTemps .= Map.empty
    localVars  .= Map.empty
    curClass   .= Nothing

withMethod :: Method -> CM IO CmmMethod -> CM IO CmmMethod
withMethod m f = do
    curMethod .= Just m
    m' <- f
    curMethod .= Nothing
    return m'

withLocalObject :: Identifier -> CM IO CmmExp -> CM IO CmmExp
withLocalObject id f = do
    localObjectType .= Just id
    e <- f
    localObjectType .= Nothing
    return e

localScope :: CM IO a -> CM IO a
localScope f = do
    s <- get
    r <- f
    localTemps .= view localTemps s
    localVars  .= view localVars  s
    return r

-- | boilerplate

curClass :: Lens' CmmScope (Maybe Class)
curClass = lens _curClass (\x y -> x { _curClass = y })

curMethod :: Lens' CmmScope (Maybe Method)
curMethod = lens _curMethod (\x y -> x { _curMethod = y })

localObjectType   :: Lens' CmmScope (Maybe Identifier)
localObjectType = lens  _localObjectType (\x y -> x { _localObjectType = y })

symbols :: Lens' CmmScope ST.MiniJavaTable
symbols = lens  _symbols (\x y -> x { _symbols = y })

cmm   :: Lens' CmmScope Cmm
cmm = lens _cmm (\x y -> x { _cmm = y })

localTemps   :: Lens' CmmScope (Map.Map Identifier CmmExp)
localTemps = lens _localTemps (\x y -> x { _localTemps = y })

curRetTemp   :: Lens' CmmScope CmmExp
curRetTemp = lens _curRetTemp (\x y -> x { _curRetTemp = y })

localVars   :: Lens' CmmScope (Map.Map Identifier Type)
localVars = lens _localVars (\x y -> x { _localVars = y })
