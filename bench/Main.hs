module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                      as DVS
import           Data.Word

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

benchPopCount1 :: [Benchmark]
benchPopCount1 =
  [
  ]

main :: IO ()
main = defaultMain benchPopCount1
