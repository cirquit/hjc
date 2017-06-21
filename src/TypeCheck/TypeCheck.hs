module TypeCheck.TypeCheck where

import qualified Data.Map                   as Map
import qualified Data.List                  as DL  (intercalate, find)
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import qualified SymbolTable   as ST
import           AST
import           TypeCheck.TCCore
import           Text.Printf
import           TypeCheck.ErrorMsgs

-- | main intro function
--
typecheck :: MiniJava -> IO TypeScope
typecheck ast = snd <$> runStateT (checkMiniJava ast) typescope
    where
        -- | running state
        --
        typescope :: TypeScope
        typescope = createTypeScope symbols

        symbols :: ST.MiniJavaTable
        symbols = ST.createSymbolTable ast

checkMiniJava :: MiniJava -> StateT TypeScope IO ()
checkMiniJava (MiniJava mc others) = do
    checkClass mc
    mapM_ checkClass others

checkClass :: Class -> StateT TypeScope IO ()
checkClass c@(Class name extends vars mets) = do
    resetClassScope
    curClass .= Just c
    checkExtention extends
    mapM_ checkVariable vars
    mapM_ checkMethod   mets

checkExtention :: Identifier -> StateT TypeScope IO ()
checkExtention id = do
    exists <- classExists id
    when (not exists) $ undefinedExtendedsClassError id

checkVariable :: Variable -> StateT TypeScope IO ()
checkVariable (Variable varType name) = do
    let id = showJC varType
    exists <- classExists id
    when (not exists) $ appendError typeNotDefinedError
  where
    typeNotDefinedError = do
        let id = showJC varType
        return $ "undefined type " ++ "\"" ++ id ++ "\" in definiton of variable " ++ "\"" ++ name ++ "\""

checkReturnType :: Type -> StateT TypeScope IO ()
checkReturnType VoidT = return ()
checkReturnType rtype = do
    let id = showJC rtype
    exists <- classExists id
    when (not exists) $ appendError typeNotDefinedError
  where
    typeNotDefinedError = do
        let id = showJC rtype
        return $ printf "Undefined type \"%s\" in the return type" id


checkMethod :: Method -> StateT TypeScope IO ()
checkMethod m@(Method name retType args body) = 
    localScope $ do
        curMethod .= Just m
        checkReturnType retType
        mapM_ checkVariable args
        mapM_ addToScope    args
        checkBody body

addToScope :: Variable -> StateT TypeScope IO ()
addToScope (Variable t id) = scope %= Map.insert id t

checkBody :: [Statement] -> StateT TypeScope IO ()
checkBody = mapM_ checkStatement

checkStatement :: Statement -> StateT TypeScope IO ()
checkStatement x@(If _ _ _)   = checkIf    x
checkStatement x@(While _ _)  = checkWhile x
checkStatement   (PrintLn e)  = checkPrintLn e
checkStatement   (Print   e)  = checkPrint   e
checkStatement   (StmExp  e)  = checkExpression e
checkStatement  (BlockStm ex) = localScope $ mapM_ checkStatement ex


checkIf :: Statement -> StateT TypeScope IO ()
checkIf (If exp stms mstms) = do
    isValidIfExpr exp  -- move to parser
    exp `shouldBeType_` BoolT
    mapM_ checkStatement stms
    mapM_ checkStatement (maybe [] id mstms)
  where
    isValidIfExpr (BlockExp _) = appendError $ return "illegal start of an if expression"
    isValidIfExpr (Assign _ _) = appendError $ return "illegal start of an if expression"
    isValidIfExpr (Return _)   = appendError $ return "illegal start of an if expression"
    isValidIfExpr _            = return ()

checkWhile :: Statement -> StateT TypeScope IO ()
checkWhile (While exp stm) = do
    isValidIfExpr exp
    exp `shouldBeType_` BoolT
    mapM_ checkStatement stm
  where
    isValidIfExpr (BlockExp _) = appendError $ return "illegal start of a while expression"
    isValidIfExpr (Assign _ _) = appendError $ return "illegal start of a while expression"
    isValidIfExpr (Return _)   = appendError $ return "illegal start of a while expression"
    isValidIfExpr _            = return ()

checkPrintLn :: Expression -> StateT TypeScope IO ()
checkPrintLn exp = do
    exp `shouldBeType_` IntT
    return ()

checkPrint :: Expression -> StateT TypeScope IO ()
checkPrint exp = do
    exp `shouldBeType_` IntT
    return ()

checkExpression :: Expression -> StateT TypeScope IO ()
checkExpression exp = unify exp >> return ()

unify :: Expression -> StateT TypeScope IO Type
unify (LitBool _)   = return BoolT
unify (LitInt  _)   = return IntT
unify (LitVar var)  = addToScope var >> return (_type var)
unify (LitStr _)    = return StringT
unify (StrArr expr) = returnIfMatched StringArrT <$> (expr `shouldBeType` IntT) 
unify (IntArr expr) = returnIfMatched IntArrT    <$> (expr `shouldBeType` IntT)
unify  This         = curClassType
unify (BlockExp xs) = head <$> mapM unify xs  -- check if the assumtion holds that the parser does not allow empty BlockExp
unify b@(BinOp _ _ _) = binOpUnify b
unify (LitIdent id) = do
   mtype <- lookupVarType id
   case mtype of
        (Just t) -> return t
        Nothing  -> do
            appendError . return $ "identifier \"" ++ id ++ "\" is undefined and thus has no type"
            return objectType

unify (NewObject id xs) = do
   mtype <- lookupTypeById id
   case mtype of
        Nothing  -> do
            appendError . return $ "class \"" ++ id ++ "\" is undefined in object instantiation" 
            return objectType
        (Just t) -> do
            when (not . null $ xs) $ do
                appendError . return $ "class \"" ++ id ++ "\" does not have any constructors with arguments than by MiniJava definition"
            return t

unify (IndexGet call ix) = do
    let allowedTypes = [IntArrT, StringArrT]
    (defined, callType) <- call `shouldBeTypes` allowedTypes
    when (not defined) $ do
        appendError . return $ "operator [] is not defined for type \"" ++ showJC callType ++ "\", allowed types are \"" ++ DL.intercalate ", " (map showJC allowedTypes)
    ix `shouldBeType_` IntT

    case callType of
        IntArrT    -> return IntT
        StringArrT -> return StringT

unify (UnOp unop expr) = do
    case unop of
        NOT -> returnIfMatched BoolT <$> expr `shouldBeType` BoolT
        _   -> do
            appendError . return $ "unary operator has no defined typechecking rules, please update the TypeCheck.unify"
            return objectType

unify (MemberGet expr id) = do
    t     <- unify expr
    mtype <- t `getGlobalMemberType` id
    case mtype of
        (Just t) -> return t
        Nothing  -> do
            appendError . return $ "member \"" ++ id ++ "\" is not defined in class \"" ++ showJC t ++ "\" in expression:\n\n         " ++ showJC expr
            return objectType

unify (Assign lhs rhs)    = do
    lhsType <- unify lhs
    returnIfMatched lhsType <$> rhs `shouldBeType` lhsType

unify m@(MethodGet expr id xs) = do
    callerType <- unify expr
    mmsymbols <- callerType `lookupMethodSymbols` id
    case mmsymbols of
        (Just msymbols) -> do
            let argTypes = map _type (view ST.arguments msymbols)
            compareArgumentCount callerType id xs argTypes
            zipWithM_ shouldBeType xs argTypes
            return $ view ST.returnType msymbols 
        Nothing         -> do
            appendError . return $ "method \"" ++ showJC callerType ++ "." ++ id ++ "\" does not exists in the class \"" ++ showJC callerType ++"\""
            return objectType

unify (Return x) = do
    retType <- curMethodType
    x `shouldBeType` retType
    return retType


-- | main unification
--
shouldBeType :: Expression -> Type -> StateT TypeScope IO Bool
shouldBeType expr t = do
    ut <- unify expr
    same <- shouldBeTypeDeep ut t
    when (not same) $ do
        appendError . return $ "type \"" ++ showJC ut ++ "\" does not match expected type \"" ++ showJC t ++ "\" in expression:\n\n         " ++ showJC expr
    return same


shouldBeTypeDeep :: Type -> Type -> StateT TypeScope IO Bool
shouldBeTypeDeep src dst = do
    let same = src == dst
    mecs <- lookupExtendedClass src
    case (same, mecs) of
         (True, _)         -> return same
         (False, Just ecs) -> shouldBeTypeDeep ecs dst
         _                 -> return False

shouldBeTypes :: Expression -> [Type] -> StateT TypeScope IO (Bool, Type)
shouldBeTypes expr ts = do
    ut <- unify expr
    let defined = ut `elem` ts
    when (not defined) $ do
            appendError . return $ "type  \"" ++ showJC ut ++ "\" does not match expected types \"" ++ (DL.intercalate ", " $ map showJC ts) ++ "\" in expression:\n\n         " ++ showJC expr
    return (defined, ut) 

shouldBeType_ :: Expression -> Type -> StateT TypeScope IO ()
shouldBeType_ e t = shouldBeType e t >> return ()

shouldBeTypes_ :: Expression -> [Type] -> StateT TypeScope IO ()
shouldBeTypes_ e ts = shouldBeTypes e ts >> return ()

returnIfMatched :: Type -> Bool -> Type
returnIfMatched t True  = t
returnIfMatched t False = objectType

-- | dirty - this should be accessible in the global typescope
--
--                 CallerType    MethodName    MethodArguments   MethodTypes
--                       \          |               |             /
compareArgumentCount :: Type -> Identifier -> [Expression] -> [Type] -> StateT TypeScope IO ()
compareArgumentCount callerType id xs args = do
    let argCount = length args
        xsCount  = length xs
        equal    = argCount == xsCount
    when (not equal) $ do
        appendError . return $
             "method \"" ++ showJC callerType ++ "." ++ id ++ "\" has  \"" ++ show argCount ++ "\"  arguments, not  \"" ++ show xsCount ++ "\""


-- | extensible binop rules unifier
--
--   table is sorted the following way:
--
--      [ Operation | Allowed Incoming Types | Return Type ] 
--
binOpUnify :: Expression -> StateT TypeScope IO Type
binOpUnify (BinOp lhs binop rhs) = do
    let binTable = [
            (MUL,   [IntT],              IntT)
           ,(DIV,   [IntT],              IntT)
           ,(MOD,   [IntT],              IntT)
           ,(PLUS,  [IntT],              IntT)
           ,(MINUS, [IntT],              IntT)
           ,(LEQ,   [IntT],              BoolT)
           ,(LE,    [IntT],              BoolT)
           ,(GEQ,   [IntT],              BoolT)
           ,(GE,    [IntT],              BoolT)
           ,(EQS,   [IntT, BoolT],       BoolT)
           ,(NEQS,  [IntT, BoolT],       BoolT)
           ,(AND,   [BoolT],             BoolT)
           ,(OR,    [BoolT],             BoolT)
           ]
    let mrules = DL.find (\(bop, _, _) -> bop == binop) binTable
    case mrules of
        (Just (op, incTs, retT)) -> checkBinOpType op incTs lhs rhs >> return retT
        Nothing               -> do
            appendError . return $ "binary operator \"" ++ showJC binop ++ "\" has no defined typechecking rules, please update the TypeCheck.unify"
            return objectType

  where
    checkBinOpType :: BinaryOp -> [Type] -> Expression -> Expression -> StateT TypeScope IO ()
    checkBinOpType binop allowedTypes lhs rhs = do
            (defined, lhsT) <- lhs `shouldBeTypes` allowedTypes
            when (not defined) $ operatorTypeMismatchError binop lhsT allowedTypes
            when (defined)     $ rhs `shouldBeType_` lhsT
