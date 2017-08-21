{-# LANGUAGE BangPatterns #-}

module Cmm.DirectedGraph(
  DirectedGraph, emptyGraph, nodes, successors,
  addNode, addEdge, outDegree, reverseGraph, toList
  ) where

import           Data.Char
import           Data.Set               (Set)
import           Data.Map               (Map)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import           Data.List              (foldl')
import           Debug.Trace            (trace)

import Cmm.X86.InstrCore

data DirectedGraph a =
   DirectedGraph {
      nodes :: Set a,
      succs :: Map a (Set a)
   }

successors :: (Ord a) => DirectedGraph a -> a -> Set a
successors g x = Map.findWithDefault Set.empty x (succs g)

outDegree :: (Ord a) => DirectedGraph a -> a -> Int
outDegree g x = Set.size $ successors g x

emptyGraph :: DirectedGraph a
emptyGraph = DirectedGraph { nodes = Set.empty, succs = Map.empty  }

addNode :: (Ord a) => DirectedGraph a -> a -> DirectedGraph a
addNode g x = g { nodes = Set.insert x (nodes g) }

addEdge :: (Ord a) => DirectedGraph a -> a -> a -> DirectedGraph a
addEdge g src dst =
   g { succs = Map.insertWith Set.union src (Set.singleton dst) (succs g) }

reverseGraph :: Ord a => DirectedGraph a -> DirectedGraph a
reverseGraph g = do
    let ns = Set.toAscList $ nodes g
    snd $ foldl' reverseRelation (g, emptyGraph) ns
  where
    -- reverseRelation :: (DirectedGraph a, DirectedGraph a) -> a -> (DirectedGraph a, DirectedGraph a)
    reverseRelation (oldGraph, newGraph) node = do
        let children   = Set.toAscList $ successors oldGraph node
            newGraph'  = foldl' addNode newGraph children
            newGraph'' = foldl' (\g c -> addEdge g c node) newGraph' children
        (oldGraph, newGraph'') 

-- | tail recursive depth first search
--
toList :: Ord a => DirectedGraph a -> a -> [a]
toList g node = reverse $ go g [node] []
  where
      -- go :: Ord a => DirectedGraph a -> [a] -> [a] -> [a]
      go g []     acc  = acc
      go g (n:ns) !acc
        | n `elem` acc = go g ns acc
        | otherwise    = do
            let children = Set.toAscList $ successors g n
            go g (children ++ ns) (n:acc)

-- | dot graph visualization
-- The output can be opened
-- with dotty (<code>dotty output.dot</code>) or converted to PDF
-- with dot (<code>dot -Tpdf output.dot > output.pdf</code>).
--  See: http://www.graphviz.org
instance (Ord a, Show a) => Show (DirectedGraph a) where
  show g = unlines $ ["digraph G {"] ++ ns ++ es ++ ["}"]
             where ns = map showAlphaNum $ Set.toAscList $ nodes g
                   es = map (\x -> show x)$ Set.toAscList $ edgeSet g

edgeSet :: (Eq a, Ord a) => DirectedGraph a -> Set (Edge a)
edgeSet g = Set.fromList ss
  where
     ss = Map.assocs (succs g) >>= \ (k, vs) -> map (edge k) $ Set.toAscList vs
     edge a b = Edge a b -- (min a b) (max a b)

data Edge a = Edge a a
  deriving (Eq, Ord)

instance Show a => Show (Edge a) where
  show (Edge a b) = 
      case (showAlphaNum a, showAlphaNum b) of
          ("", _) -> ""
          (_ , "") -> ""
          (a,b)    -> a ++ " -> " ++ b 

showAlphaNum :: (Show a) => a -> String
showAlphaNum = concatMap readable . filter (/= '%') . show
    where
        readable ' ' = "_"
        readable ',' = "_"
        readable ':' = "_"
        readable '-' = "_"
        readable '+' = "_"
        readable '[' = "×"
        readable ']' = "×"
        readable '(' = "_"
        readable ')' = "_"
        readable '$' = ""
        readable c   = [c]
