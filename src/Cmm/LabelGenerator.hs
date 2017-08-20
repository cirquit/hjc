{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}
{-
 - Representation of names of temps and labels
 -}
module Cmm.LabelGenerator (
  Temp(..), Label, mkLabel, mkNamedTemp,
  MonadNameGen(..),
  NameGen, runNameGen,
  NameGenT, runNameGenT
  ) where

import Control.Monad.State
import Control.Monad.Identity

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

-- | Name generation monad transformer.
newtype NameGenT m a = NameGenT (StateT ([Temp], [Label]) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Name generation monad.
type NameGen a = NameGenT Identity a

runNameGen :: NameGen a -> a
runNameGen = runIdentity . runNameGenT

instance (Monad m) => MonadNameGen (NameGenT m) where
  nextTemp' = NameGenT $ do (t:ts, ls) <- get; put (ts, ls); return t
  avoid' av =
    NameGenT $
      do (ts, ls) <- get
         put (filter (\t -> not (show t `elem` (map show av))) ts, ls)
         return ()
  nextLabel' = NameGenT $ do (ts, l:ls) <- get; put (ts, ls); return l

runNameGenT :: (Monad m) => NameGenT m a -> m a
runNameGenT (NameGenT x) =
   evalStateT x ([Temp i | i<-[0..]], ["L_" ++ (show i) | i <- [(0::Int)..]])
