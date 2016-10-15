module Demo where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy hiding (Point, Vector)
import           Pipes ((>->))
import qualified Pipes.Prelude as P
import           Pipes.Random (finite, endless)
import           SpatialSort

---

-- | A sin wave.
wave :: Vector Point
wave = V.generate 100 (\n -> Point (fromIntegral n / 3) $ sin (fromIntegral n / 3))

-- | x^3
x3 :: Vector Point
x3 = V.fromList $ map (\n -> Point n $ n ** 3) [-100, -99 .. 100 ]

-- | Randomize a Vector.
random :: Vector a -> IO (Vector a)
random = fmap V.fromList . P.toListM . finite

-- | A random scatter plot.
scatter :: IO (Vector Point)
scatter = do
  xs <- P.toListM $ endless (V.fromList [1 .. 1000]) >-> P.take 5000
  ys <- P.toListM $ endless (V.fromList [1 .. 1000]) >-> P.take 5000
  let v = V.fromList $ map (uncurry Point) $ zip xs ys
  pure v

poly :: Vector Point
poly = V.fromList
  [ Point 1 1
  , Point 2 1
  , Point 2 2
  , Point 1 2
  , Point 1 11
  , Point 2 11
  , Point 2 12
  , Point 1 12
  , Point 11 11
  , Point 12 11
  , Point 12 12
  , Point 11 12
  , Point 11 1
  , Point 12 1
  , Point 12 2
  , Point 11 2
  ]

-- | Render a given Vector, given its target filename.
png :: String -> Vector Point -> IO ()
png s v = toFile def s $ do
  layout_title .= s
  plot $ line "x" [V.toList $ V.map (\(Point a b) -> (a,b)) v]

-- | Produce charts for a random scatter plot.
scatters :: IO ()
scatters = do
  s <- scatter
  png "scatter-large-raw.png" s
  png "scatter-large-sorted.png" $ spatialSort s

polys :: IO ()
polys = do
  png "polys-raw.png" poly
  ps <- random poly
  png "polys-random.png" ps
  png "polys-sorted.png" $ spatialSort ps
