{-# LANGUAGE Safe #-}

module Invert.Reexport
  ( -- * Hashable
    Hashable,

    -- * Generic
    Generic,

    -- * GEnum
    GEnum,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Generics.Deriving (GEnum)
