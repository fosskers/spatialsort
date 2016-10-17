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

-- | Total length of line of points.
pathLen :: Vector Point -> Float
pathLen v = sum . V.map (uncurry distance) $ V.zip v (V.tail v)

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
  let s' = spatialSort s
  png "scatter-large-raw.png" s
  png "scatter-large-sorted.png" s'
  putStrLn $ "LEN REDUC: " ++ show (pathLen s / pathLen s')

{-}
bestN :: IO ()
bestN = do
  s <- scatter
  mapM_ (f s) [4 .. 20]
  where f s n = do
          let s' = spatialSort n s
          putStrLn $ "N: " ++ show n ++ " " ++ show (pathLen s / pathLen s')
-}
waves :: IO ()
waves = do
  png "sin-raw.png" wave
  r <- random wave
  png "sin-random.png" r
  png "sin-sorted.png" $ spatialSort r

polys :: IO ()
polys = do
  png "polys-raw.png" poly
  ps <- random poly
  png "polys-random.png" ps
  png "polys-sorted.png" $ spatialSort ps
