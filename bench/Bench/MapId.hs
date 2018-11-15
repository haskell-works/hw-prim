module Bench.MapId where

import Data.Word

import qualified Data.Vector.Storable as DVS

mapId :: DVS.Vector Word64 -> DVS.Vector Word64
mapId = DVS.map id
{-# INLINE mapId #-}
