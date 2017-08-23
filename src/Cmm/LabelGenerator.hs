{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}
{-
 - Representation of names of temps and labels
 -}
module Cmm.LabelGenerator (
  Temp(..), Label, mkLabel, mkNamedTemp,
  MonadNameGen(..),
  NameGen, runNameGen,
  NameGenT, runNameGenT, evalNameGenT, runWithNameStateT
  ) where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.IORef


-- | A type to represent temporaries.
--
-- The constructors are private.  The usual way of obtaining a
-- temporary is to generate it freshly using 'nextTemp' from the name
-- generation monad 'MonadNameGen'.
data Temp = NamedTemp String
          | Temp Int deriving (Eq, Ord)

instance Show Temp where
  show (Temp i) = "t" ++ show i
  show (NamedTemp s) = s

-- | Generates a temporary with a fixed name 's'. This function must
-- be used with care, to avoid name clashes. The function 'nextTemp'
-- guarantees freshness only relative to the names it has generated
-- itself.
mkNamedTemp :: String -> Temp
mkNamedTemp = NamedTemp

-- | A type of symbolic labels.
type Label = String

-- | Constructs a label with a fixed name. Names containing the
-- character '$' are reserved for internal use.
mkLabel :: String -> Label
mkLabel l | '$' `elem` l =
              error $ "Label \"" ++ l ++ "\" contains reserver character '$'."
          | otherwise = 'L':l

-- | Name generation monad
class Monad m => MonadNameGen m where
  -- | Generates a fresh temporary. The returned temporary is
  -- guaranteed to be different from all the ones returned previously
  -- and the ones give to 'avoid'.
  nextTemp' :: m Temp

  -- | Declare that a list of temps must be avoided by 'nextTemp'.
  -- 'nextTemp' will not return a temp that was passed to 'avoid'.
  avoid' :: [Temp] -> m ()

  -- | Generates a fresh label.
  nextLabel' :: m Label

  -- -- | returns the current state (used to atomic access for parallelism)
  -- getNameState' :: m ([Temp], [Label])

  -- -- | returns the current state (used to atomic access for parallelism)
  -- setNameState' :: ([Temp], [Label]) -> m ()


-- | Name generation monad transformer.
newtype NameGenT m a = NameGenT (StateT (IORef ([Temp], [Label])) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Name generation monad.
type NameGen a = NameGenT IO a

runNameGen :: NameGen a -> IO a
runNameGen = runNameGenT

-- | had to create an atomic interface for NameGenT based on MonadIO
instance (Monad m, MonadIO m) => MonadNameGen (NameGenT m) where
  nextTemp' = NameGenT $ do
      stateIORef <- get
      t <- liftIO $ atomicModifyIORef stateIORef (\((t:ts), ls) -> ((ts, ls), t))
      return t
  avoid' av = 
    NameGenT $ do
      stateIORef <- get
      (ts, ls) <- liftIO $ readIORef stateIORef
      let ts' = filter (\t -> not (show t `elem` (map show av))) ts
      _ <- liftIO $ atomicModifyIORef stateIORef (\(ts, ls) -> ((ts', ls), ()))
      return ()
  nextLabel' = NameGenT $ do
      stateIORef <- get
      l  <- liftIO $ atomicModifyIORef stateIORef (\(ts, l:ls) -> ((ts, ls), l))
      return l

runNameGenT :: (Monad m, MonadIO m) => NameGenT m a -> m a
runNameGenT (NameGenT f) = do
   state <- liftIO $ newIORef ([Temp i | i<-[0..]], ["L_" ++ (show i) | i <- [(0::Int)..]])
   evalStateT f state

evalNameGenT :: (Monad m, MonadIO m) => NameGenT m a -> m (a, IORef ([Temp], [Label]))
evalNameGenT (NameGenT f) = do
   state <- liftIO $ newIORef ([Temp i | i<-[0..]], ["L_" ++ (show i) | i <- [(0::Int)..]])
   runStateT f state

runWithNameStateT :: (Monad m, MonadIO m) => IORef ([Temp], [Label]) -> NameGenT m a -> m a
runWithNameStateT s (NameGenT f) = do
   evalStateT f s