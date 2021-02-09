{-# language Safe #-}

module Invert.Reexport
  (
    {- * Hashable -} Hashable,
    {- * Generic  -} Generic,
    {- * GEnum    -} GEnum
  ) where

import Data.Hashable     ( Hashable )
import Generics.Deriving ( GEnum )
import GHC.Generics      ( Generic )
