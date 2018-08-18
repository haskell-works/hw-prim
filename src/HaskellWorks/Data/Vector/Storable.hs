module HaskellWorks.Data.Vector.Storable where

import Data.Semigroup ((<>))
import Data.Word

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

padded :: Int -> DVS.Vector Word8 -> DVS.Vector Word8
padded n v = v <> DVS.replicate ((n - DVS.length v) `max` 0) 0
{-# INLINE padded #-}
