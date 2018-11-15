module Bench.MapAccumL where

import Data.Word

import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Vector.Storable as DVS

mapAccumL :: DVS.Vector Word64 -> DVS.Vector Word64
mapAccumL = snd . DVS.mapAccumL f 0
  where f a b = (a + b, a * b)
