module SpatialSort where
{-}
  ( Point(..)
  , spatialSort
  , distance
  ) where
-}

import           Data.List (sortBy)
import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Insertion as I
import           Lens.Micro

---

data Point = Point { x :: Float, y :: Float } deriving (Show, Ord, Eq)

-- | Discover two kernels to group the other points to.
-- Assumption: The `Vector` has at least five elements.
kernels :: Vector Point -> (Point, Point)
kernels v = (avg (V.head v) mid, avg (V.last v) mid)
  where mid = v V.! (V.length v `div` 2)
        avg (Point a b) (Point c d) = Point ((a + c) / 2) ((b + d) / 2)

kernels2 :: Vector Point -> (Point, Point)
kernels2 v = (V.head v, V.last v)

-- | Assumption: The Vector isn't empty.
centroid :: Vector Point -> Point
centroid v = Point x' y'
  where (x', y') = V.foldl (\(ax,ay) (Point a b) -> (ax + a, ay + b)) (0, 0) v & both %~ (/ l)
        l = fromIntegral $ V.length v

-- | Euclidean distance.
distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ dx ** 2 + dy ** 2
  where dx = x p1 - x p2
        dy = y p1 - y p2

-- | Give a decent one-dimensional order to Points which are spatially close.

spatialSort0 :: Vector Point -> Vector Point
spatialSort0 v | V.length v == 2 = V.fromList [minimum v, maximum v]
               | V.length v < 2 = v
               | otherwise = spatialSort0 l <> spatialSort0 r
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels2 v

spatialSort1 :: Vector Point -> Vector Point
spatialSort1 v | V.length v == 0 = v
               | V.length v < 6 = V.modify I.sort v
               | otherwise = spatialSort1 l <> spatialSort1 r
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels v

spatialSort2 :: Vector Point -> Vector Point
spatialSort2 v | V.length v == 0 = v
               | V.length v < 6 = V.modify (I.sortBy (\a b -> compare (distance a c) (distance b c))) v
               | otherwise = spatialSort2 l <> spatialSort2 r
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels v
        c = centroid v

spatialSort3 :: Vector Point -> Vector Point
spatialSort3 v | V.length v == 0 = v
               | V.length v < 6 = V.modify (I.sortBy (\a b -> compare (distance a c) (distance b c))) v
               | otherwise = fuse (spatialSort3 l) (spatialSort3 r)
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels v
        c = centroid v

spatialSort4 :: Vector Point -> Vector Point
spatialSort4 v | V.length v == 0 = v
               | V.length v < 6 = V.modify I.sort v
               | otherwise = fuse (spatialSort4 l) (spatialSort4 r)
  where (l, r) = V.partition (\p -> distance p kl < distance p kr) v
        (kl, kr) = kernels v
        c = centroid v


-- | Fuse two lines by whichever end points are closest.
fuse :: Vector Point -> Vector Point -> Vector Point
fuse v1 v2 | V.null v1 = v2
           | V.null v2 = v1
           | otherwise = snd . head $ sortBy (\(d0, _) (d1, _) -> compare d0 d1) pairs
           where pairs = [ (distance (V.last v1) (V.head v2), v1 <> v2)
                         , (distance (V.last v1) (V.last v2), v1 <> V.reverse v2)
                         , (distance (V.head v1) (V.head v2), V.reverse v1 <> v2)
                         , (distance (V.head v1) (V.last v2), v2 <> v1) ]
