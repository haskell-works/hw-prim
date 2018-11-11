module Bench.MapOnStateId where

import Control.Monad.Trans.State
import Data.Vector.Fusion.Util   (Id (..))
import Data.Word

import qualified Data.Vector.Storable as DVS

mapOnStateId :: DVS.Vector Word64 -> DVS.Vector Word64
mapOnStateId v = unId go
  where go :: Id (DVS.Vector Word64)
        go = fst <$> runStateT ho 0
        ho :: StateT Word64 Id (DVS.Vector Word64)
        ho = DVS.mapM return v
{-# INLINE mapOnStateId #-}
