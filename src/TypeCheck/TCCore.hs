module TypeCheck.TCCore where

import qualified Data.Map                     as Map
-- import Control.Monad (foldM)
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Data.Maybe (fromJust)

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


-- | helper to save the current scope with the class and method
--   where the error happend
--
appendError :: StateT TypeScope IO String -> StateT TypeScope IO ()
appendError errMsg = do
    error <- TypeError <$> curClassName <*> curMethodName <*> errMsg
    errors %= (++ [error])

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

lookupType :: Identifier -> StateT TypeScope IO (Maybe Type)
lookupType id = do
    mjt <- view symbols <$> get
    return $ id `ST.lookupType` mjt


-- | valid java code has to be in a class, no need to test on existence
--
curClassName :: StateT TypeScope IO Identifier
curClassName = _className . fromJust . view curClass <$> get

-- | method may not exist, using Maybe Functor
--
curMethodName :: StateT TypeScope IO (Maybe Identifier)
curMethodName = (_methodName <$>) <$> view curMethod <$> get


