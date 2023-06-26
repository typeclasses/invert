{-# LANGUAGE Safe #-}

module Map where

import Data.Eq (Eq)
import Data.Foldable qualified as Seq
  ( toList,
  )
import Data.HashMap.Strict qualified as HashMap
  ( empty,
    lookup,
    singleton,
    union,
    unionWith,
  )
import Data.Hashable (Hashable)
import Data.Map.Strict qualified as OrdMap
  ( empty,
    lookup,
    singleton,
    union,
    unionWith,
  )
import Data.Maybe (Maybe, maybe)
import Data.Ord (Ord)
import Data.Sequence qualified as Seq
  ( singleton,
    (><),
  )

data Map f a b = forall map.
  Map
  { empty :: map,
    singleton :: a -> b -> map,
    union :: map -> map -> map,
    lookup :: map -> a -> f b
  }

type SingleMap = Map Maybe

type MultiMap = Map []

hashSingleMap :: (Eq a, Hashable a) => SingleMap a b
hashSingleMap = Map {empty, singleton, union, lookup}
  where
    empty = HashMap.empty
    singleton = HashMap.singleton
    union = HashMap.union
    lookup m a = HashMap.lookup a m

hashMultiMap :: (Eq a, Hashable a) => MultiMap a b
hashMultiMap = Map {empty, singleton, union, lookup}
  where
    empty = HashMap.empty
    singleton = \a b -> HashMap.singleton a (Seq.singleton b)
    union = HashMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (HashMap.lookup a m)

ordSingleMap :: Ord a => SingleMap a b
ordSingleMap = Map {empty, singleton, union, lookup}
  where
    empty = OrdMap.empty
    singleton = OrdMap.singleton
    union = OrdMap.union
    lookup m a = OrdMap.lookup a m

ordMultiMap :: Ord a => MultiMap a b
ordMultiMap = Map {empty, singleton, union, lookup}
  where
    empty = OrdMap.empty
    singleton = \a b -> OrdMap.singleton a (Seq.singleton b)
    union = OrdMap.unionWith (Seq.><)
    lookup = \m a -> maybe [] Seq.toList (OrdMap.lookup a m)
