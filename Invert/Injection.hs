module Invert.Injection (Inversion, eq, ord, hash) where

import Invert.Tools

import Data.Eq (Eq)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import qualified Data.List as List

type Inversion a b =
    [a] -- ^ A complete list of all the values of the domain
    -> (a -> b) -- ^ An injective function to invert
    -> (b -> Maybe a) -- ^ The inverse of the given injection

eq :: Eq b => Inversion a b
eq = viaMap assocList

ord :: Ord b => Inversion a b
ord = viaMap ordMap

hash :: (Eq b, Hashable b) => Inversion a b
hash = viaMap hashMap

viaMap :: SingleMap b a -> Inversion a b
viaMap Map{ empty, singleton, union, lookup } as f = lookup map
  where
    entry a = singleton (f a) a
    map = foldl' union empty (List.map entry as)
