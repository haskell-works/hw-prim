{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.AsVector64s
  ( AsVector64s(..)
  ) where

import Control.Monad.ST
import Data.Word
import Foreign.ForeignPtr

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

class AsVector64s a where
  -- | Represent the value as a list of Vector of 'n' Word64 chunks.  The last chunk will
  -- also be of the specified chunk size filled with trailing zeros.
  asVector64s :: Int -> a -> [DVS.Vector Word64]

instance AsVector64s LBS.ByteString where
  asVector64s n = asVector64s n . LBS.toChunks
  {-# INLINE asVector64s #-}

instance AsVector64s [BS.ByteString] where
  asVector64s = bytestringsToVectors
  {-# INLINE asVector64s #-}

bytestringsToVectors :: Int -> [BS.ByteString] -> [DVS.Vector Word64]
bytestringsToVectors n bss = DVS.createT $ go (byteStringToVector8 <$> bss)
  where go :: [DVSM.MVector s Word8] -> ST s [DVSM.MVector s Word64]
        go ts = do
          result <- buildOneVector n ts
          case result of
            Just (us, v) -> (v:) <$> go us
            Nothing      -> return []
{-# INLINE bytestringsToVectors #-}

buildOneVector :: forall s. Int -> [DVSM.MVector s Word8] -> ST s (Maybe ([DVSM.MVector s Word8], DVSM.MVector s Word64))
buildOneVector n ss = case dropWhile ((== 0) . DVSM.length) ss of
  [] -> return Nothing
  cs -> do
    v64 <- DVSM.unsafeNew n
    let v8 = DVSM.unsafeCast v64
    rs  <- go cs v8
    return (Just (rs, v64))
  where go :: [DVSM.MVector s Word8] -> DVSM.MVector s Word8 -> ST s [DVSM.MVector s Word8]
        go ts v = if DVSM.length v > 0
          then case ts of
            (u:us) -> do
              if DVSM.length u <= DVSM.length v
                then case DVSM.splitAt (DVSM.length u) v of
                  (va, vb) -> do
                    DVSM.copy va u
                    go us vb
                else case DVSM.splitAt (DVSM.length v) u of
                  (ua, ub) -> do
                    DVSM.copy v ua
                    return (ub:us)
            [] -> do
              DVSM.set v 0
              return []
          else return ts
        {-# INLINE go #-}
{-# INLINE buildOneVector #-}

byteStringToVector8 :: BS.ByteString -> DVSM.MVector s Word8
byteStringToVector8 bs = case BS.toForeignPtr bs of
  (fptr, off, len) -> DVSM.unsafeFromForeignPtr (castForeignPtr fptr) off len
{-# INLINE byteStringToVector8 #-}
