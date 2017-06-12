module TypeCheck.TCCore where

import qualified Data.Map                     as Map
-- import Control.Monad (foldM)
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Data.Maybe (fromJust)
import           Control.Applicative --

import           AST
import qualified SymbolTable as ST

-- boilerplate
io :: MonadIO m => IO a -> m a 
io = liftIO

type Scope = Map.Map Identifier Type

data TypeScope = TypeScope
    {   _symbols   :: ST.MiniJavaTable
      , _scope     :: Scope
      , _curClass  :: Maybe Class
      , _curMethod :: Maybe Method
      , _errors    :: [TypeError]
    }

createTypeScope :: ST.MiniJavaTable -> TypeScope
createTypeScope mjt = TypeScope
    {   _symbols   = mjt
      , _scope     = Map.empty
      , _curClass  = Nothing
      , _curMethod = Nothing
      , _errors    = [] 
    }

data TypeError = TypeError 
    {   _class    :: Identifier
      , _method   :: Maybe Identifier
      , _errorMsg :: String
    } deriving (Eq)


symbols :: Lens' TypeScope ST.MiniJavaTable
symbols = lens _symbols (\x y -> x { _symbols = y })

scope :: Lens' TypeScope Scope
scope = lens _scope (\x y -> x { _scope = y })

curClass :: Lens' TypeScope (Maybe Class) 
curClass = lens _curClass (\x y -> x { _curClass = y })

curMethod :: Lens' TypeScope (Maybe Method) 
curMethod = lens _curMethod (\x y -> x { _curMethod = y })

errors :: Lens' TypeScope [TypeError]
errors = lens _errors (\x y -> x { _errors = y })

successful :: TypeScope -> Bool
successful ts = null $ _errors ts

-- | utility functions for our state
-- |



resetClassScope :: StateT TypeScope IO ()
resetClassScope = do
   scope     .= Map.empty
   curClass  .= Nothing
   curMethod .= Nothing

-- | reset local variable scope, forward errors
--
localScope :: StateT TypeScope IO a -> StateT TypeScope IO ()
localScope f = do
    oldScope <- get
    errors .= []
    f
    newScope <- get
    put oldScope
    -- scope  .= (view scope oldScope)
    errors %= (++ (view errors newScope))

classExists :: Identifier -> StateT TypeScope IO Bool
classExists id = do
    mjt <- view symbols <$> get
    return $ id `ST.classExistsIn` mjt

-- | by id
--
lookupTypeById :: Identifier -> StateT TypeScope IO (Maybe Type)
lookupTypeById id = do
    mjt <- view symbols <$> get
    return $ id `ST.lookupType` mjt

lookupClassSymbolsById :: Identifier -> StateT TypeScope IO (Maybe ST.ClassSymbols)
lookupClassSymbolsById id = do
    mjt <- view symbols <$> get
    return $ id `ST.lookupClassSymbols` mjt

lookupExtendedClassById :: Identifier -> StateT TypeScope IO (Maybe Type)
lookupExtendedClassById id = do
    mjt <- view symbols <$> get
    let mes = view ST.extendsSym =<< (id `ST.lookupClassSymbols` mjt)
    case mes of
         Nothing -> return Nothing
         (Just es) -> lookupTypeById es
-- |                    class id       method id
--
lookupMethodSymbolsById :: Identifier -> Identifier -> StateT TypeScope IO (Maybe ST.MethodSymbols)
lookupMethodSymbolsById cid mid = do
    mcs <- lookupClassSymbolsById cid
    mms <- (>>= ST.lookupMethodSymbols mid) <$> pure mcs
    case (mms, mcs >>= view ST.extendsSym) of
         (Just ms, Just es) -> return $ Just ms
         (Nothing, Just es) -> lookupMethodSymbolsById es mid
         _                  -> return Nothing


-- | prefer local scope over global scope - this is used to implement shadowing
--
lookupVarType :: Identifier -> StateT TypeScope IO (Maybe Type)
lookupVarType id = do
    mtypeLS <- getLocalMemberType id
    mtypeGS <- (\cls -> getGlobalMemberType cls id) =<< curClassType
    return $ mtypeLS <|> mtypeGS

-- | TODO:
--
--      lookup in extended classes
--
getGlobalMemberType :: Type -> Identifier -> StateT TypeScope IO (Maybe Type)
getGlobalMemberType t id = do
    mtype <- lookupType t
    case mtype of
         (Just t) -> return mtype
         Nothing   -> do
            mT <- lookupExtendedClass t
            case mT of
                Nothing -> return Nothing
                (Just eCT) -> getGlobalMemberType eCT id
    where
      lookupType ct = (>>= ST.getGlobalMemberType id) <$> lookupClassSymbols ct

getLocalMemberType :: Identifier -> StateT TypeScope IO (Maybe Type)
getLocalMemberType id = Map.lookup id . view scope <$> get

-- | by type
--
lookupType :: Type -> StateT TypeScope IO (Maybe Type)
lookupType = lookupTypeById . showJC 

lookupClassSymbols :: Type -> StateT TypeScope IO (Maybe ST.ClassSymbols)
lookupClassSymbols = lookupClassSymbolsById . showJC

lookupExtendedClass :: Type -> StateT TypeScope IO (Maybe Type)
lookupExtendedClass = lookupExtendedClassById . showJC

lookupMethodSymbols :: Type -> Identifier -> StateT TypeScope IO (Maybe ST.MethodSymbols)
lookupMethodSymbols = lookupMethodSymbolsById . showJC


-- | valid java code has to be in a class, no need to test on existence
--
curClassName :: StateT TypeScope IO Identifier
curClassName = _className . fromJust . view curClass <$> get

curClassType :: StateT TypeScope IO Type
curClassType = IdT <$> curClassName

-- | method may not exist, using Maybe Functor
--
curMethodName :: StateT TypeScope IO (Maybe Identifier)
curMethodName = (_methodName <$>) <$> view curMethod <$> get


-- | use only in unify, invarinat holds that we can't defined expressions out of methods 
--
curMethodType :: StateT TypeScope IO Type
curMethodType = (fromJust . (_methodRetType <$>)) <$> view curMethod <$> get
