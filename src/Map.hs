module Map where

import Data.Eq       ( Eq, (==) )
import Data.Foldable ( foldl' )
import Data.Hashable ( Hashable )
import Data.Maybe    ( Maybe (Just, Nothing), maybe )
import Data.Ord      ( Ord )

import qualified Data.Foldable
    as Seq (toList)

import qualified Data.HashMap.Strict
    as HashMap (lookup, singleton, empty, union, unionWith)

import qualified Data.Map.Strict
    as OrdMap (lookup, singleton, empty, union, unionWith)

import qualified Data.Sequence
    as Seq (empty, singleton, (><), (|>))

data Map f a b = forall map.
  Map
    { empty :: map
    , singleton :: a -> b -> map
    , union :: map -> map -> map
    , lookup :: map -> a -> f b
    }

type SingleMap = Map Maybe

type MultiMap = Map []

seqSingleMap :: Eq a => SingleMap a b
seqSingleMap = Map{ empty, singleton, union, lookup }
  where
    empty = Seq.empty
    singleton a b = Seq.singleton (a, b)
    union = (Seq.><)
    lookup l a' = foldl' f Nothing l
      where
        f Nothing (a, b) = if (a == a') then Just b else Nothing
        f (Just b) _ = Just b

seqMultiMap :: Eq a => MultiMap a b
seqMultiMap = Map{ empty, singleton, union, lookup }
  where
    empty = Seq.empty
    singleton a b = Seq.singleton (a, b)
    union = (Seq.><)
    lookup l a' = Seq.toList (foldl' f Seq.empty l)
      where
        f bs (a, b) = if (a == a') then (Seq.|>) bs b else bs

hashSingleMap :: (Eq a, Hashable a) => SingleMap a b
hashSingleMap = Map{ empty, singleton, union, lookup }
  where
    empty = HashMap.empty
    singleton = HashMap.singleton
    union = HashMap.union
    lookup m a = HashMap.lookup a m

hashMultiMap :: (Eq a, Hashable a) => MultiMap a b
hashMultiMap = Map{ empty, singleton, union, lookup }
  where
    empty = HashMap.empty
    singleton = \a b -> HashMap.singleton a (Seq.singleton b)
    union = HashMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (HashMap.lookup a m)

ordSingleMap :: Ord a => SingleMap a b
ordSingleMap = Map{ empty, singleton, union, lookup }
  where
    empty = OrdMap.empty
    singleton = OrdMap.singleton
    union = OrdMap.union
    lookup m a = OrdMap.lookup a m

ordMultiMap :: Ord a => MultiMap a b
ordMultiMap = Map{ empty, singleton, union, lookup }
  where
    empty = OrdMap.empty
    singleton = \a b -> OrdMap.singleton a (Seq.singleton b)
    union = OrdMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (OrdMap.lookup a m)
