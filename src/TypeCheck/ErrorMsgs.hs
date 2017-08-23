module TypeCheck.ErrorMsgs where

import qualified Data.List    as DL
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens
import           Text.Printf                (printf)

import qualified SymbolTable   as ST
import           AST
import           TypeCheck.TCCore

-- | helper to save the current scope with the class and method
--   where the error happend
--
appendError :: StateT TypeScope IO String -> StateT TypeScope IO ()
appendError errMsg = do
    error <- TypeError <$> curClassName <*> curMethodName <*> errMsg
    errors %= (++ [error])


-- | class error messages
--
--
undefinedExtendedsClassError :: Identifier -> StateT TypeScope IO ()
undefinedExtendedsClassError id = appendError . return $ do
    printf "undefined extended class \"%s\"" id


-- | operator error messages

operatorTypeMismatchError :: BinaryOp -> Type -> [Type] -> StateT TypeScope IO ()
operatorTypeMismatchError op t allowedTypes = appendError . return $ do
    printf  "operator %s is not defined for type \"%s\", allowed types are \"%s\"" (showJC op) (showJC t) allowedTypesS
  where
    allowedTypesS = DL.intercalate ", " (map showJC allowedTypes)