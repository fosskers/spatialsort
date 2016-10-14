module SpatialSort
  ( Point(..)
  , spatialSort
  ) where

import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as V

---

data Point = Point { x :: Float, y :: Float } deriving (Show, Ord, Eq)

-- | Discover two kernels to group the other points to.
-- Assumption: The `Vector` has at least two elements.
kernels :: Vector Point -> (Point, Point)
kernels v = (V.head v, V.last v)  -- pretty naive

-- | Euclidean distance.
distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ dx ** 2 + dy ** 2
  where dx = x p1 - x p2
        dy = y p1 - y p2

-- | Give a decent one-dimensional order to Points which are spatially close.
spatialSort :: Vector Point -> Vector Point
spatialSort v | V.length v == 2 = V.fromList [minimum v, maximum v]
              | V.length v < 2 = v
              | otherwise = spatialSort l <> spatialSort r
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels v
