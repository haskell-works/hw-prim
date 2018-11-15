module Bench.MapOnId where

import Data.Vector.Fusion.Util (Id (..))
import Data.Word

import qualified Data.Vector.Storable as DVS

mapOnId :: DVS.Vector Word64 -> DVS.Vector Word64
mapOnId v = unId go
  where go :: Id (DVS.Vector Word64)
        go = DVS.mapM return v
{-# INLINE mapOnId #-}
