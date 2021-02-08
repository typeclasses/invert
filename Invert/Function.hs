module Invert.Function (Inversion, eq, ord, hash) where

import Invert.Tools

import Data.Eq (Eq)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Ord (Ord)
import qualified Data.List as List

type Inversion a b =
    [a] -- ^ A complete list of all the values of the domain
    -> (a -> b) -- ^ The function to invert
    -> (b -> [a]) -- ^ The inverse of the given function

eq :: Eq b => Inversion a b
eq = viaMap assocMultimap

ord :: Ord b => Inversion a b
ord = viaMap ordMultimap

hash :: (Eq b, Hashable b) => Inversion a b
hash = viaMap hashMultimap

viaMap :: MultiMap b a -> Inversion a b
viaMap Map{ empty, singleton, union, lookup } as f = lookup map
  where entry a = singleton (f a) a
        map = foldl' union empty (List.map entry as)
