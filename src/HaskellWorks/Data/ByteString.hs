{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.ByteString
  ( chunkedBy
  , ToByteString(..)
  ) where

import Data.Word

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

class ToByteString a where
  toByteString :: a -> BS.ByteString

instance ToByteString BS.ByteString where
  toByteString = id

instance ToByteString (DVS.Vector Word8) where
  toByteString v = case DVS.unsafeToForeignPtr v of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word16) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word32) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word64) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

-- | Chunk a @bs into list of smaller byte strings of no more than @n elements
chunkedBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs
