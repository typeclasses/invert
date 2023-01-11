{-# language Safe #-}

module Invert
  (
    -- * Overview
    -- $overview

    -- * 1. Varieties of function
    function, bijection, injection, surjection,

    -- * 2. Inversion strategies
    linearSearchLazy, linearSearchStrict, binarySearch, hashTable,

    -- * 3. Domain enumeration
    enumBounded, genum,

    -- * The Strategy type
    Strategy,
    -- $strategyCreation
    strategyAll, strategyOneAndAll,

    -- * Re-exports
    -- $reexports
    module Invert.Reexport

  ) where

import Invert.Reexport

import qualified Map
import Map (Map (Map))

import qualified Vector

import Data.Eq            ( Eq, (==) )
import Data.Foldable      ( foldl' )
import Data.Function      ( (.) )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Data.Maybe         ( Maybe (Just, Nothing), fromMaybe, listToMaybe )
import Data.Ord           ( Ord )
import Data.Tuple         ( uncurry )
import Prelude            ( error )
import Prelude            ( Enum, enumFromTo )
import Prelude            ( Bounded, minBound, maxBound )

import qualified Data.List         as List  ( lookup, map )
import qualified Data.Maybe        as List  ( mapMaybe )
import qualified Generics.Deriving as GEnum ( genum )

{- $overview

There are three considerations when you’re inverting a function:

  1. Is it an injection, a surjection, both (a bijection), or neither?
  2. What data structure do you want to use for efficient lookups?
  3. Can you produce a list of all values in the function’s domain?

=== 1. What sort of function do you have?

This question determines the type of the function’s inverse.

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
apply the function to each element of the domain, one by one.
We call this a /linear search/ because the time required for each
application has a linear correspondence with the size of the domain.

  * 'linearSearchStrict' works by precomputing a strict sequence
    of tuples, one for each value of the domain.

  * 'linearSearchLazy' precomputes nothing at all.
    It is possible to use this strategy when the domain is infinite.

Our other two strategies, 'binarySearch' and 'hashTable',
work by building data structures that allow more efficient lookups.

  * 'binarySearch' precomputes a binary search tree;
    the codomain must belong to the 'Ord' class.

  * 'hashTable' precomputes a hash table;
    the codomain must belong to the 'Hashable' class.

The 'Hashable' class comes from "Data.Hashable" in the @hashable@ package.
The class is re-exported by "Invert", which you may find convenient if
your primary motivation for deriving 'Hashable' is to invert a function.

=== 3. How will you enumerate the domain?

Inverting a function @(a -> b)@ requires having a list of all
possible values of domain @(a)@; from this, we can apply the
function to every value to produce a list of tuples that
completely describes the function.

We offer two suggestions for automatically producing this list:

  * 'enumBounded' uses two stock-derivable classes, 'Enum' and 'Bounded'.
  * 'genum' uses GHC generics; it requires deriving 'Generic' and 'GEnum'.

The 'Generic' class comes from "GHC.Generics", and the 'GEnum' class
comes from "Generics.Deriving" in the @generic-deriving@ package.
Both classes are re-exported by "Invert", which you may find convenient
if your primary motivation for deriving 'GEnum' is to invert a function.

-}

function ::
    Strategy a b
    -> [a]        -- ^ A complete list of all the values of the domain.
    -> (a -> b)   -- ^ The function to invert.
    -> (b -> [a]) -- ^ The inverse of the given function.

bijection ::
    Strategy a b
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
    Strategy a b
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
    Strategy a b
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

{- |

    An inversion strategy is an approach for producing
    the inverse of an @(a -> b)@ function.

    All strategies produce the same results, but they
    have operational differences that affect performance.

-}

data Strategy a b =
  Strategy
    ([(b, a)] -> b -> Maybe a)
    ([(b, a)] -> b -> [a])

{- $strategyCreation

    === Defining your own strategies

    If you want to design your own strategy instead
    of using one provided by this module, use either
    'strategyAll' or 'strategyOneAndAll'.

-}

strategyAll ::
    ([(b, a)] -> b -> [a]) -- ^ Find all matches
    -> Strategy a b
strategyAll all = strategyOneAndAll one all
  where
    one bas b = listToMaybe (all bas b)

strategyOneAndAll ::
    ([(b, a)] -> b -> Maybe a) -- ^ Find the first match
    -> ([(b, a)] -> b -> [a]) -- ^ Find all matches
    -> Strategy a b
strategyOneAndAll = Strategy

inverseEntries :: [a] -> (a -> b) -> [(b, a)]
inverseEntries as f = List.map (\a -> (f a, a)) as

mapStrategy :: Map Maybe b a -> Map [] b a -> Strategy a b
mapStrategy one all = Strategy (f one) (f all)
  where
    f Map{ Map.empty, Map.singleton, Map.union, Map.lookup } =
        lookup . foldl' union empty . List.map (uncurry singleton)

{- |

    A function inversion strategy that precomputes nothing at all.
    It is possible to use this strategy when the domain is infinite.

-}

linearSearchLazy :: Eq b => Strategy a b
linearSearchLazy = Strategy one all
  where
    one bas b = List.lookup b bas
    all bas b = List.mapMaybe (sndIfFstEq b) bas

{- |

    A function inversion strategy that works by precomputing a
    strict sequence of tuples, one for each value of the domain.

    For larger functions, it may be preferable to use 'binarySearch' or
    'hashTable' instead to get a more efficient inverse.

-}

linearSearchStrict :: Eq b => Strategy a b
linearSearchStrict = strategyAll f
  where
    f bas b = Vector.toList (Vector.mapMaybe (sndIfFstEq b) v)
      where
        v = Vector.fromList bas

sndIfFstEq :: Eq b => b -> (b, a) -> Maybe a
sndIfFstEq x (b, a) = if b == x then Just a else Nothing

{- |

    A function inversion strategy that works by precomputing
    a binary search tree. The data structure imposes the
    requirement that the codomain belongs to the 'Ord' class.

-}

binarySearch :: Ord b => Strategy a b
binarySearch = mapStrategy Map.ordSingleMap Map.ordMultiMap

{- |

    A function inversion strategy that works by precomputing
    a hash table. The data structure imposes the requirement
    that the codomain belongs to the 'Hashable' class.

-}

hashTable :: (Eq b, Hashable b) => Strategy a b
hashTable = mapStrategy Map.hashSingleMap Map.hashMultiMap

-- |
-- 'enumBounded' can be a convenient way to enumerate
-- the domain for a function that you want to invert.
-- It uses two stock-derivable classes, 'Enum' and 'Bounded'.
--
-- To derive the required typeclass instances, add the
-- following deriving clause to the type’s definition:
--
--   > deriving (Enum, Bounded)
--

enumBounded :: (Enum a, Bounded a) => [a]
enumBounded = enumFromTo minBound maxBound

-- |
-- 'genum' uses GHC generics; it requires deriving 'Generic'
-- and 'GEnum'. The 'Generic' class comes from "GHC.Generics",
-- and the 'GEnum' class comes from "Generics.Deriving" in the
-- @generic-deriving@ package.
--
-- To derive the required typeclass instances, enable the
-- following language extensions:
--
--   > {-# language DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
--
-- Then add the following deriving clauses to the type’s definition:
--
--   > deriving stock Generic
--   > deriving anyclass GEnum
--

genum :: GEnum a => [a]
genum = GEnum.genum

{- $reexports

This module provides a few definitions that come directly from
other packages. These are here to let you conveniently derive
'Hashable' and 'GEnum' with only the "Invert" module imported.

List of re-exports:

  - __'Hashable'__ (for the 'hashTable' inversion strategy)
  - __'Generic'__ and __'GEnum'__ (for the 'genum' domain enumeration approach)

-}
