{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.AsVector64
  ( AsVector64(..)
  ) where

import Data.Word

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Storable   as DVS

class AsVector64 a where
  asVector64 :: a -> DVS.Vector Word64

instance AsVector64 (DVS.Vector Word64) where
  asVector64 = id

instance AsVector64 BS.ByteString where
  asVector64 bs = DVS.unsafeCast v
    where bsLen = BS.length bs
          v = DVS.constructN ((BS.length bs + 7) `div` 8) go :: DVS.Vector Word8
          go :: DVS.Vector Word8 -> Word8
          go u = let ui = DVS.length u in if ui < bsLen then BSU.unsafeIndex bs ui else 0
