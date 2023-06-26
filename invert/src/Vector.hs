{-# LANGUAGE Trustworthy #-}

module Vector (fromList, toList, mapMaybe) where

import Data.Maybe (Maybe)
import Data.Vector (Vector)
import Data.Vector qualified as V

fromList :: [a] -> Vector a
fromList = V.fromList

toList :: Vector a -> [a]
toList = V.toList

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe = V.mapMaybe
