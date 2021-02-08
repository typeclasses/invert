module Invert.Tools where

import Data.Eq (Eq, (==))
import Data.Hashable (Hashable)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import qualified Data.HashMap.Strict as HashMap (lookup, singleton, empty, union, unionWith)
import qualified Data.Map.Strict as OrdMap (lookup, singleton, empty, union, unionWith)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Seq (toList)
import Data.Ord (Ord)
import Data.Foldable (foldl')

data Map f a b = forall map.
  Map
    { empty :: map
    , singleton :: a -> b -> map
    , union :: map -> map -> map
    , lookup :: map -> a -> f b
    }

type SingleMap = Map Maybe
type MultiMap = Map []

assocList :: Eq a => SingleMap a b
assocList = Map{ empty, singleton, union, lookup }
  where
    empty = Seq.empty
    singleton a b = Seq.singleton (a, b)
    union = (Seq.><)
    lookup l a' = foldl' f Nothing l
      where
        f Nothing (a, b) = if (a == a') then Just b else Nothing
        f (Just b) _ = Just b

assocMultimap :: Eq a => MultiMap a b
assocMultimap = Map{ empty, singleton, union, lookup }
  where
    empty = Seq.empty
    singleton a b = Seq.singleton (a, b)
    union = (Seq.><)
    lookup l a' = Seq.toList (foldl' f Seq.empty l)
      where
        f bs (a, b) = if (a == a') then (Seq.|>) bs b else bs

hashMap :: (Eq a, Hashable a) => SingleMap a b
hashMap = Map{ empty, singleton, union, lookup }
  where
    empty = HashMap.empty
    singleton = HashMap.singleton
    union = HashMap.union
    lookup m a = HashMap.lookup a m

hashMultimap :: (Eq a, Hashable a) => MultiMap a b
hashMultimap = Map{ empty, singleton, union, lookup }
  where
    empty = HashMap.empty
    singleton = \a b -> HashMap.singleton a (Seq.singleton b)
    union = HashMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (HashMap.lookup a m)

ordMap :: Ord a => SingleMap a b
ordMap = Map{ empty, singleton, union, lookup }
  where
    empty = OrdMap.empty
    singleton = OrdMap.singleton
    union = OrdMap.union
    lookup m a = OrdMap.lookup a m

ordMultimap :: Ord a => MultiMap a b
ordMultimap = Map{ empty, singleton, union, lookup }
  where
    empty = OrdMap.empty
    singleton = \a b -> OrdMap.singleton a (Seq.singleton b)
    union = OrdMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (OrdMap.lookup a m)
