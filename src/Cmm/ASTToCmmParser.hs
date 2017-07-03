module Cmm.ASTToCmmParser where

import qualified Data.Map                   as Map
import qualified Data.List                  as DL
import qualified Data.Bool                  as B
import           Data.Int
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           AST
import           Cmm.LabelGenerator                 ( Temp, Label, mkLabel, mkNamedTemp, MonadNameGen(..)
                                                    , NameGen, runNameGen, NameGenT, runNameGenT
                                                    )
import           Cmm.CAST
import qualified SymbolTable                as ST

ast2cmms :: MiniJava -> IO String
ast2cmms ast = cmm2str <$> ast2cmm ast

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

-- | C-- Monad
--
--  Based on the NameGenT so we can use the `nextLabel` with internal state without contaminating the CmmScope
--  Outer Transformer is the StateT which holds the CmmScope
--
--
type CM m a = StateT CmmScope (NameGenT m) a 

-- | main intro function
--
ast2cmm :: MiniJava -> IO Cmm
ast2cmm ast = (view cmm . snd) <$> runNameGenT (runStateT (parseMiniJavaCmm ast) cmmScope)
    where
        -- | running state
        --
        cmmScope :: CmmScope
        cmmScope = CmmScope
            { _curClass       = Nothing
            , _curMethod      = Nothing
            , _localObjectType  = Nothing
            , _symbols        = symbols
            , _cmm            = []
            , _curRetTemp     = TEMP $ mkNamedTemp "mainReturnTemp"
            , _localTemps     = Map.empty
            , _localVars      = Map.empty
            }

        -- | computing this twice, here and in TypeCheck.hs, this should be cached
        symbols :: ST.MiniJavaTable
        symbols = ST.createSymbolTable ast


parseMiniJavaCmm :: MiniJava -> CM IO ()
parseMiniJavaCmm (MiniJava mc oc) = do
    mapM_ parseClassCmm oc
    parseMainClassCmm mc

-- | currently a single method is supported
--
parseMainClassCmm :: Class -> CM IO ()
parseMainClassCmm cls = withClass cls $ do
    m <- withArgLen 0 . withName "Lmain" <$> parseMethodCmm (head $ _methods cls)
    addZeroReturn m >>= addMethod

parseClassCmm :: Class -> CM IO ()
parseClassCmm cls = withClass cls $ do
     addClassVariablesToScope (_variables cls)
     mapM_ (\m -> parseMethodCmm m >>= addMethod) (_methods cls)


addClassVariablesToScope :: [Variable] -> CM IO ()
addClassVariablesToScope vars = do
    mapM_ insertMemoryPaddedVar $ zip vars [1..] 
  where
    insertMemoryPaddedVar :: (Variable, Int32) -> CM IO ()
    insertMemoryPaddedVar (v, i) = do
        let memExp = MEM $ BINOP PLUS_C (PARAM 0) (CONST (i * 4))
        localTemps %= Map.insert (_variableName v) memExp 
        localVars  %= Map.insert (_variableName v) (_type v)

parseMethodCmm :: Method -> CM IO CmmMethod
parseMethodCmm meth = withMethod meth $ do
    (t, exp) <- nextTempTE
    curRetTemp .= exp
    n <- methodNameCmm
    CmmMethod <$> methodNameCmm
              <*> methodArgLength
              <*> methodBodyCmm
              <*> pure t

methodNameCmm :: CM IO String
methodNameCmm = do
    (Just cls)  <- view curClass  <$> get
    (Just meth) <- view curMethod <$> get
    return $ 'L' : (_className cls) ++ '$' : (_methodName meth)

-- | every method has one more argument - the object pointer 'this'
--
methodArgLength :: CM IO Int
methodArgLength =  do
    (Just meth) <- view curMethod <$> get
    return . (1 +) . length $ _methodArguments meth

methodBodyCmm :: CM IO [CmmStm]
methodBodyCmm = do
    (Just meth) <- view curMethod <$> get
    localScope $ do
        addArgumentsToScope
        mapM stmParserC (_methodBody meth)


-- | MAIN Statement parser
--
--
stmParserC :: Statement -> CM IO CmmStm
stmParserC s@(If _  _  _) = ifParserC s
stmParserC s@(While _  _) = whileParserC s
stmParserC s@(BlockStm _) = blockstmParserC s 
stmParserC   (PrintLn  e) = printlnParserC e >>= moveTemp
stmParserC   (Print    e) = printParserC e   >>= moveTemp
stmParserC   (StmExp   e) = expParserC e     >>= moveTemp

-- generates 3 labels, if-branch, else-branch and end-case
-- 
ifParserC :: Statement -> CM IO CmmStm
ifParserC (If e b1 b2) = do
    ce <- expParserC e
    (trueL, trueLS)       <- nextLabelLS 
    (falseL, falseLS)     <- nextLabelLS 
    (endL, endLS, endLE)  <- nextLabelLSE
    let cjmp   = CJUMP EQ_C ce (CONST 1) trueL falseL
        endjmp = JUMP endLE [endL]
    trueCS  <- SEQ <$> mapM stmParserC b1
    falseCS <- SEQ <$> mapM stmParserC (maybe [] id b2)
    return $ SEQ [cjmp, trueLS, trueCS, endjmp, falseLS, falseCS, endLS] 

whileParserC :: Statement -> CM IO CmmStm
whileParserC (While e b) = do
    ce <- expParserC e
    (startL, startLS, startLE) <- nextLabelLSE
    (whileL, whileLS)          <- nextLabelLS 
    (endL, endLS, endLE)       <- nextLabelLSE
    let cjmp   = CJUMP EQ_C ce (CONST 1) whileL endL
        startjmp = JUMP startLE [startL]
    whileCS  <- SEQ <$> mapM stmParserC b
    return $ SEQ [startLS, cjmp, whileLS, whileCS, startjmp, endLS]

blockstmParserC :: Statement -> CM IO CmmStm
blockstmParserC (BlockStm stms) = SEQ <$> mapM stmParserC stms

printlnParserC :: Expression -> CM IO CmmExp
printlnParserC e = do
    let l = NAME $ mkLabel "_println_int"
    CALL l <$> (:[]) <$> expParserC e

printParserC :: Expression -> CM IO CmmExp
printParserC e = do
    let l = NAME $ mkLabel "_print_char"
    CALL l <$> (:[]) <$> expParserC e

-- | MAIN Expression parser
--
-- object parser
expParserC :: Expression -> CM IO CmmExp
expParserC (NewObject id es) = do
    localObjectType .= Just id
    classMemory <- calculateClassMemoryCost id
    callAlloc (CONST classMemory)

expParserC (MethodGet callerE mid es) = do
    ptr  <- expParserC callerE
    args <- mapM expParserC es
    res <- CALL <$> methodLabel mid callerE <*> pure (ptr : args)
    localObjectType .= Nothing
    return res

expParserC (LitBool b) = return . CONST $ B.bool 0 1 b
expParserC (LitInt i)  = return . CONST $ fromIntegral i
expParserC (StrArr e)  = error $ "hjc:ASTToCmmParser:expParserC - String arrays are not implemented yet"
expParserC (IntArr e)  = do
   arrLenE     <- expParserC e
   arrMemoryCE <- calculateIntArrMemoryCost arrLenE
   arrTE       <- nextTempE
   allocMemory <- callAlloc arrMemoryCE
   let allocStm   = MOVE arrTE       allocMemory
       fillLength = MOVE (MEM arrTE) arrLenE 
   return $ ESEQ (SEQ [allocStm,  fillLength]) arrTE 

expParserC (IndexGet e ix) = do
    callerCE <- expParserC e -- this is the pointer to the array, and the first element is the length
    ixCE     <- expParserC ix
    
    indexByteTempE <- nextTempE
    let indexByteOffsetCE = MOVE indexByteTempE $
                               BINOP MUL_C
                                   (BINOP PLUS_C ixCE (CONST 1))
                                   (CONST 4)

    (lengthValidL, lengthValidLS)     <- nextLabelLS
    (lengthInvalidL, lengthInvalidLS) <- nextLabelLS

--      length - index <= 1
    let cjump = CJUMP LE_C (CONST 1) (BINOP MINUS_C (MEM callerCE) ixCE) lengthValidL lengthInvalidL

    invalidCS <- moveTemp =<< callRaise =<< indexOutOfBounds

    memberTE <- nextTempE
    let validCS = MOVE memberTE (BINOP PLUS_C callerCE indexByteTempE)

    return $ ESEQ (SEQ [indexByteOffsetCE, cjump, lengthInvalidLS, invalidCS, lengthValidLS, validCS]) (MEM memberTE)


-- this pointer is always the first argument of the method
expParserC This = do
    (Just c) <- view curClass <$> get
    localObjectType .= Just (_className c)
    return $ PARAM 0

-- local definiton of a variable is saved to the local temps
expParserC (LitVar v) = do
    vars <- view localTemps <$> get
    case Map.lookup (_variableName v) vars of
        (Just tempExp) -> do
            return tempExp
        Nothing        -> do
            tempExp <- nextTempE
            localTemps %= Map.insert (_variableName v) tempExp
            localVars  %= Map.insert (_variableName v) (_type v) 
            return tempExp

-- identifier has to reference a temporary (locally defined variables or method arguments)
expParserC (LitIdent id) = do
    ltm <- view localTemps <$> get
    let mt = Map.lookup id ltm
    case mt of
        (Just tempExp) -> return tempExp
        Nothing        -> error $ "hjc:ASTToCmmParser:expParserC - Temporary with id \"" ++ id ++ "\" could not be found"

expParserC (Assign lhs rhs) = do
    lhsE <- expParserC lhs 
    stm <- MOVE <$> expParserC lhs <*> expParserC rhs -- assumption has to hold that any return of lhs should return a temporary (or a param)
    return $ ESEQ (SEQ [stm]) lhsE

expParserC (BinOp e1 op e2) = do
    ce1 <- expParserC e1
    ce2 <- expParserC e2
    case op of
       PLUS  -> return $ BINOP PLUS_C  ce1 ce2
       MINUS -> return $ BINOP MINUS_C ce1 ce2
       MUL   -> return $ BINOP MUL_C   ce1 ce2
       DIV   -> return $ BINOP DIV_C   ce1 ce2

       EQS  -> compareWith EQ_C ce1 ce2
       LE   -> compareWith LT_C ce1 ce2
       GE   -> compareWith GT_C ce1 ce2
       LEQ  -> compareWith LE_C ce1 ce2
       GEQ  -> compareWith GE_C ce1 ce2
       NEQS -> compareWith NE_C ce1 ce2

       AND  -> andExpParserC ce1 ce2
       OR   -> orExpParserC  ce1 ce2
       o@_   -> error $ "hjc:ASTToCmmParser:expParserC - binary operation " ++ showJC o ++ " is not implemented yet"

expParserC (UnOp op e) = do
    ce <- expParserC e
    case op of
        NOT -> compareWith LT_C ce (CONST 1) -- true = CONST 1, false = CONST 0
        o@_ -> error $  "hjc:ASTToCmmParser:expParserC - unary operation " ++ showJC o ++ " is not implemented yet"

-- TODO check assumpton that blockexpr is always a single expression
expParserC (BlockExp exps) = do
    if (length exps == 1)
        then
            expParserC (head exps)
        else
            error $ "hjc:ASTToCmmParser:expParserC - BlockExp has more than one expression: " ++ show (length exps) ++ ", " ++ concatMap showJC exps

expParserC (Return exp) = do
    stm <- MOVE <$> (view curRetTemp <$> get) <*> expParserC exp
    return $ ESEQ stm (CONST 42)

-- implementing length member by hand, arrays is a concept of our compiler
-- at the pointer of the array lies the length member
expParserC (MemberGet exp "length") = do
    ce <- expParserC exp
    return (MEM ce)

expParserC e@_ = do
    io $ print $ "hjc:ASTToCmmParser:expParserC - " ++ show e ++ " is not implemented yet"
    return $ CONST 404

-- | takes the method id and assumes that there is a new object local id defined
--
methodLabel :: Identifier -> Expression -> CM IO CmmExp
methodLabel mid exp = do
    moid  <- view localObjectType <$> get
    lvars <- view localVars <$> get
    case (moid, exp) of
        (_, LitIdent id) -> do
            case Map.lookup id lvars of
                Nothing -> error $ "hjc:ASTToCmmParser:methodLabel - couldn't find class label " ++ id ++ " in " ++ show lvars
                (Just t) -> do
                    return . NAME $ mkLabel (showJC t) ++ '$' : mid
        (Just oid, _)    -> return . NAME $ mkLabel oid ++ '$' : mid
        (_, _) -> do
            (Just cls)  <- view curClass  <$> get
            return . NAME $ mkLabel (_className cls) ++ '$' : mid

-- | utils
--
withName :: String -> CmmMethod -> CmmMethod
withName name m = m { cmmMethodName = name }

withArgLen :: Int -> CmmMethod -> CmmMethod
withArgLen l m = m { cmmArgLength = l } 

-- | monadic utils
--
addMethod :: CmmMethod -> CM IO ()
addMethod m = cmm %= (++ [m])


compareWith :: CmmRelOp -> CmmExp -> CmmExp -> CM IO CmmExp
compareWith op ce1 ce2 = do
    tempE <- nextTempE
    (trueL,   trueLS) <- nextLabelLS
    (falseL, falseLS) <- nextLabelLS
    (endL, endLS, endLE)  <- nextLabelLSE
    
    let cjmp = CJUMP op ce1 ce2 trueL falseL
        endjmp  = JUMP endLE [endL]
        trueCS  = MOVE tempE (CONST 1)
        falseCS = MOVE tempE (CONST 0)

    return $ ESEQ (SEQ [cjmp, trueLS, trueCS, endjmp,
                              falseLS, falseCS, endLS]) tempE


-- | lazy or parser
--
orExpParserC :: CmmExp -> CmmExp -> CM IO CmmExp
orExpParserC ce1 ce2 = do

    tempE <- nextTempE
    (trueL,   trueLS) <- nextLabelLS
    (falseL1, falseLS1) <- nextLabelLS
    (falseL2, falseLS2) <- nextLabelLS
    (endL, endLS, endLE)  <- nextLabelLSE
    
    let cjmpE1 = CJUMP EQ_C ce1 (CONST 1) trueL falseL1
        cjmpE2 = CJUMP EQ_C ce2 (CONST 1) trueL falseL2
        endjmp  = JUMP endLE [endL]
        
        trueCS  = MOVE tempE (CONST 1)
        falseCS = MOVE tempE (CONST 0)

    return $ ESEQ (SEQ [cjmpE1, trueLS, trueCS, endjmp,
                        falseLS1,cjmpE2, falseLS2, falseCS, endLS]) tempE

-- | lazy and  
--
andExpParserC :: CmmExp -> CmmExp -> CM IO CmmExp
andExpParserC ce1 ce2 = do

    tempE <- nextTempE
    (nextL,   nextLS) <- nextLabelLS
    (trueL,   trueLS) <- nextLabelLS
    (falseL, falseLS) <- nextLabelLS
    (endL, endLS, endLE)  <- nextLabelLSE
    
    let cjmpE1 = CJUMP EQ_C ce1 (CONST 1) nextL falseL
        cjmpE2 = CJUMP EQ_C ce2 (CONST 1) trueL falseL
        endjmp  = JUMP endLE [endL]
        
        trueCS  = MOVE tempE (CONST 1)
        falseCS = MOVE tempE (CONST 0)

    return $ ESEQ (SEQ [cjmpE1, nextLS, cjmpE2, trueLS, trueCS,
                                endjmp, falseLS, falseCS, endLS]) tempE

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

-- | Declare that a list of temps must be avoided by 'nextTemp'.
--  'nextTemp' will not return a temp that was passed to 'avoid'.
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


io :: MonadIO m => IO a -> m a 
io = liftIO
