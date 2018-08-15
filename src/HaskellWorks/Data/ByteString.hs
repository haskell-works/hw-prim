{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.ByteString
  ( chunkedBy
  , ToByteString(..)
  , rechunkSegments
  , rechunkSegmentsPadded
  ) where

import Data.Semigroup ((<>))
import Data.Word

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

class ToByteString a where
  toByteString :: a -> BS.ByteString

instance ToByteString BS.ByteString where
  toByteString = id
  {-# INLINE toByteString #-}

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
{-# INLINE chunkedBy #-}

rechunkSegments :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkSegments multiple = go
  where go (bs:bss) = case BS.length bs of
              bsLen -> if bsLen < multiple
                then case multiple - bsLen of
                  bsNeed -> case bss of
                    (cs:css) -> case BS.length cs of
                      csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                      csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                      _     | otherwise       ->                            go ((bs <> cs)       :css)
                    [] -> [bs]
                else case (bsLen `div` multiple) * multiple of
                  bsCroppedLen -> if bsCroppedLen == bsLen
                    then bs:go bss
                    else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
        go [] = []

rechunkSegmentsPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkSegmentsPadded multiple = go
  where go (bs:bss) = case BS.length bs of
              bsLen -> if bsLen < multiple
                then case multiple - bsLen of
                  bsNeed -> case bss of
                    (cs:css) -> case BS.length cs of
                      csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                      csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                      _     | otherwise       ->                            go ((bs <> cs)       :css)
                    [] -> [bs <> BS.replicate bsNeed 0]
                else case (bsLen `div` multiple) * multiple of
                  bsCroppedLen -> if bsCroppedLen == bsLen
                    then bs:go bss
                    else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
        go [] = []

