module Bench.MapAccumLFusable where

import Data.Word

import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Vector.Storable as DVS

mapAccumLFusable :: DVS.Vector Word64 -> DVS.Vector Word64
mapAccumLFusable = snd . DVS.mapAccumLFusable f 0
  where f a b = (a + b, a * b)
