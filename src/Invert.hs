module Invert
  (
    -- * How to invert a function
    -- $how

    -- * 1. Types of functions to invert
    function, bijection, injection, surjection,

    -- * 2. Strategies for inverting
    linearSearchLazy, linearSearchStrict, binarySearch, hashTable,
    -- ** What exactly is a Strategy?
    Strategy, strategy,

    -- * 3. Ways to enumerate domains
    enumBounded, genum,

    -- * Re-reexports
    -- ** For @genum@
    Generic, GEnum,
    -- ** For @hashTable@
    Hashable

  ) where

import qualified Map
import Map (Map (Map))

import Data.Eq            ( Eq, (==) )
import Data.Foldable      ( foldl' )
import Data.Function      ( (.) )
import Data.Hashable      ( Hashable )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Data.Maybe         ( Maybe (Just, Nothing), fromMaybe, listToMaybe )
import Data.Ord           ( Ord )
import Data.Tuple         ( uncurry )
import Generics.Deriving  ( GEnum )
import GHC.Generics       ( Generic )
import Prelude            ( error )
import Prelude            ( Enum, enumFromTo )
import Prelude            ( Bounded, minBound, maxBound )

import qualified Data.List         as List  ( lookup, map )
import qualified Data.Maybe        as List  ( mapMaybe )
import qualified Generics.Deriving as GEnum ( genum )


---  How function inversion works  ---

{- $how

There are three things you need to decide when you're inverting a function.

=== 1. What sort of function do you have?

This question determines the type of the function's inverse.

For a function @(a -> b)@, we call @(a)@ its /domain/, and @(b)@ its /codomain/.

  * In general, when you invert a 'function' of type @(a -> b)@,
    the type of the inverse is @(b -> [a])@.
    The result is a list because it contains all domain values that
    map to a given codomain value; there may be none, one, or many.

  * If your function @(a -> b)@ is a 'bijection',
    you can invert it to get a function @(b -> a)@.
    Bijections are quite pleasing in this way.

  * If no two domain values map to the same codomain value,
    then your function is an 'injection',
    and it has an inverse of type @(b -> 'Maybe' a)@.

  * If every codomain value has some domain value that maps to it,
    then your function is a 'surjection',
    and it has an inverse of type @(b -> 'NonEmpty' a)@.

You are responsible for determining which is appropriate for a particular
situation: 'function', 'bijection', 'injection', or 'surjection'.
Choose carefully; the wrong choice may produce an inverse which is
partial or incorrect.

=== 2. How can we produce a reasonably efficient inversion?

The simplest inversion strategies, 'linearSearchLazy' and 'linearSearchStrict',
apply the function to each element of the codomain, one by one.
We call this a /linear search/ because the time required for each
application has a linear correspondence with the size of the codomain.

  * 'linearSearchStrict' works by precomputing a strict sequence
    of @(b, a)@ pairs, one for each value of the codomain.

  * 'linearSearchLazy' precomputes nothing at all.
    It is possible to use this stategy when the codomain is infinite.

Our other two strategies, 'binarySearch' and 'hashTable',
work by building data structures that allow more efficient lookups.

  * 'binarySearch' precomputes a binary search tree;
    the codomain must belong to the 'Ord' class.

  * 'hashTable' precomputers a hash table;
    the codomain must belong to the 'Hashable' class.

The 'Hashable' class comes from "Data.Hashable" in the @hashable@ package.
The class is re-exported by "Invert", which you may find convenient if
your primary motivation for deriving 'Hashable' is to invert a function.

=== 3. How will you enumerate the domain?

Inverting a function @(a -> b)@ requires having a list of all
possible values of domain @(a)@; from this, we can apply the
function to every value to produce a list of @(a, b)@ pairs.
This list completely describes the function.

We suggest two approaches for automatically producing this list:

  * 'enumBounded' uses two stock-derivable classes, 'Enum' and 'Bounded'.
  * 'genum' uses GHC generics; it requires deriving 'Generic' and 'GEnum'.

The 'Generic' class comes from "GHC.Generics", and the 'GEnum' class
comes from "Generics.Deriving" in the @generic-deriving@ package.
Both classes are re-exported by "Invert", which you may find convenient
if your primary motivation for deriving 'GEnum' is to invert a function.

-}


---  1. Types of functions to invert  ---

function ::
    Strategy b a
    -> [a]        -- ^ A complete list of all the values of the domain.
    -> (a -> b)   -- ^ The function to invert.
    -> (b -> [a]) -- ^ The inverse of the given function.

bijection ::
    Strategy b a
    -> [a]
                -- ^ A complete list of all the values of the domain.
    -> (a -> b)
                -- ^ The function to invert.
                --   __This function must be bijective!__
                --   This means that every value in the codomain has
                --   exactly one value in the domain that maps to it.
    -> (b -> a)
                -- ^ The inverse of the given function.

injection ::
    Strategy b a
    -> [a]
                -- ^ A complete list of all the values of the domain.
    -> (a -> b)
                -- ^ The function to invert.
                --   __This function must be injective!__
                --   This means that no two values in the domain map
                --   to the same value of the codomain.
    -> (b -> Maybe a)
                -- ^ The inverse of the given function.

surjection ::
    Strategy b a
    -> [a]
                -- ^ A complete list of all the values of the domain.
    -> (a -> b)
                -- ^ The function to invert.
                --   __This function must be surjective!__
                --   This means that every value in the codomain has
                --   at least one value in the domain that maps to it.
    -> (b -> NonEmpty a)
                -- ^ The inverse of the given function.

function (Strategy _ s) as f = s (inverseEntries as f)
injection (Strategy s _) as f = s (inverseEntries as f)
bijection (Strategy s _) as f = finagle . s (inverseEntries as f)
  where finagle = fromMaybe (error "Not a bijection!")
surjection (Strategy _ s) as f = finagle . s (inverseEntries as f)
  where finagle = fromMaybe (error "Not a surjection!") . nonEmpty


---  2. Strategies for inverting  ---

data Strategy a b =
  Strategy
    ([(a, b)] -> a -> Maybe b)
    ([(a, b)] -> a -> [b])

strategy :: ([(a, b)] -> a -> [b]) -> Strategy a b
strategy many = Strategy one many
  where
    one abs a = listToMaybe (many abs a)

inverseEntries :: [a] -> (a -> b) -> [(b, a)]
inverseEntries as f = List.map (\a -> (f a, a)) as

mapStrategy :: Map Maybe a b -> Map [] a b -> Strategy a b
mapStrategy x y = Strategy (f x) (f y)
  where f Map{ Map.empty, Map.singleton, Map.union, Map.lookup } =
            lookup . foldl' union empty . List.map (uncurry singleton)

-- | A function inversion strategy that precomputes nothing at all.
-- It is possible to use this stategy when the codomain is infinite.
linearSearchLazy :: Eq a => Strategy a b
linearSearchLazy = Strategy one many
  where one abs a' = List.lookup a' abs
        many abs a' = List.mapMaybe f abs
            where f (a, b) = if a == a' then Just b else Nothing

-- | A function inversation strategy that works by precomputing a
-- strict sequence of @(b, a)@ pairs, one for each value of the codomain.
-- For larger functions, it may be preferable to use 'binarySearch' or
-- 'hashTable' instead to get a more efficient inverse.
linearSearchStrict :: Eq a => Strategy a b
linearSearchStrict = mapStrategy Map.seqSingleMap Map.seqMultiMap

-- | A function inversion strategy that works by precomputing
-- a binary search tree. The data structure imposes the
-- requirement that the codomain belongs to the 'Ord' class.
binarySearch :: Ord a => Strategy a b
binarySearch = mapStrategy Map.ordSingleMap Map.ordMultiMap

-- | A function inversion strategy that works by precomputing
-- a hash table. The data structure imposes the requirement
-- that the codomain belongs to the 'Hashable' class.
hashTable :: (Eq a, Hashable a) => Strategy a b
hashTable = mapStrategy Map.hashSingleMap Map.hashMultiMap


---  3. Ways to enumerate domains  ---

enumBounded :: (Enum b, Bounded b) => [b]
enumBounded = enumFromTo minBound maxBound

genum :: GEnum b => [b]
genum = GEnum.genum
