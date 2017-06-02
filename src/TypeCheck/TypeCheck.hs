module TypeCheck.TypeCheck where

import qualified Data.Map                   as Map
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when)

import qualified SymbolTable   as ST
import           AST
import           TypeCheck.TCCore

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
    when (not exists) $ appendError extendsError
  where
    extendsError = do
        return $ "Undefined extended class " ++ "\"" ++ id ++ "\"" 

checkVariable :: Variable -> StateT TypeScope IO ()
checkVariable (Variable varType name) = do
    let id = showJC varType 
    exists <- classExists id
    when (not exists) $ appendError typeNotDefinedError
  where
    typeNotDefinedError = do
        let id = showJC varType
        return $ "Undefined type " ++ "\"" ++ id ++ "\" in definiton of variable " ++ "\"" ++ name ++ "\""

checkReturnType :: Type -> StateT TypeScope IO ()
checkReturnType VoidT = return ()
checkReturnType rtype = do
    let id = showJC rtype 
    exists <- classExists id
    when (not exists) $ appendError typeNotDefinedError
  where
    typeNotDefinedError = do
        let id = showJC rtype
        return $ "Undefined type " ++ "\"" ++ id ++ "\" in the return type"


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
    return ()
  where
    isValidIfExpr (BlockExp _) = appendError $ return "illegal start of an if expression"
    isValidIfExpr (Assign _ _) = appendError $ return "illegal start of an if expression"
    isValidIfExpr (Return _)   = appendError $ return "illegal start of an if expression"
    isValidIfExpr _            = return ()

checkWhile :: Statement -> StateT TypeScope IO ()
checkWhile (While exp stm) =  return ()

checkPrintLn :: Expression -> StateT TypeScope IO ()
checkPrintLn exp = return ()

checkPrint :: Expression -> StateT TypeScope IO ()
checkPrint exp = return ()

checkExpression :: Expression -> StateT TypeScope IO ()
checkExpression exp = return ()

shouldBeType :: Expression -> Type -> StateT TypeScope IO Bool
shouldBeType expr t = do
    ut <- unify expr
    let same = ut == t
    when (not same) $ do
        appendError . return $ "type \"" ++ showJC ut ++ "\" does not match expected type \"" ++ showJC t ++ "\" in expression:\n\n         " ++ showJC expr ++ "\n"
    return same

shouldBeType_ :: Expression -> Type -> StateT TypeScope IO ()
shouldBeType_ e t = shouldBeType e t >> return ()

unify :: Expression -> StateT TypeScope IO Type
unify (LitBool _)   = return BoolT
unify (LitInt  _)   = return IntT
unify (LitVar var)  = return $ _type var
unify (LitStr _)    = return StringT
unify (LitIdent id) = return $ IdT id
unify (StrArr e)    = do
    match <- e `shouldBeType` IntT
    if match
        then return StringArrT
        else return objectType
unify (IntArr e)    = do
    match <- e `shouldBeType` IntT
    if match
        then return IntArrT
        else return objectType
-- unify (NewObject id xs) = "new " ++ id ++ "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
-- unify (IndexGet x x') = showJC x ++ "[" ++ showJC x' ++ "]"
-- unify (MemberGet x id) = showJC x ++ "." ++ id
-- unify (MethodGet x id xs) = showJC x ++ "." ++ id ++ "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
-- unify (Assign x x') = showJC x ++ " = " ++ showJC x'
-- unify (BinOp x b x') = showJC x ++ " " ++ showJC b ++ " " ++ showJC x'
-- unify (UnOp u x) = showJC x
-- unify (Length x) = showJC x ++ ".length"
-- unify This = "this"
-- unify (BlockExp xs) = "( " ++ concat (intersperse "," (map showJC xs)) ++ " )"
-- unify (Return x) = "return " ++ showJC x
-- unify (LitStr x) = show x