module Invert.Domain where

import Prelude (Enum, Bounded, enumFromTo, minBound, maxBound)
import Generics.Deriving (GEnum)
import qualified Generics.Deriving as GEnum

genum :: GEnum b => [b]
genum = GEnum.genum

enumBounded :: (Enum b, Bounded b) => [b]
enumBounded = enumFromTo minBound maxBound
