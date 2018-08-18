module HaskellWorks.Data.Vector.Storable where

import Data.Word

import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS
import qualified Foreign.ForeignPtr       as F
import qualified Foreign.Marshal.Unsafe   as F
import qualified Foreign.Ptr              as F

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

padded :: Int -> DVS.Vector Word8 -> DVS.Vector Word8
padded n v = F.unsafeLocalState $ do
  let (srcFptr, srcOffset, srcLen) = DVS.unsafeToForeignPtr v
  tgtFptr <- BSI.mallocByteString n
  F.withForeignPtr srcFptr $ \srcPtr -> do
    F.withForeignPtr tgtFptr $ \tgtPtr -> do
      let dataLen = n `min` srcLen
      let dataPtr = srcPtr `F.plusPtr` srcOffset
      let padPtr = dataPtr `F.plusPtr` dataLen
      BSI.memcpy tgtPtr dataPtr (n `min` srcLen)
      _ <- BSI.memset padPtr 0 $ fromIntegral ((n - srcLen) `max` 0)
      return $ DVS.unsafeFromForeignPtr tgtFptr 0 n
