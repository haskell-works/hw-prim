{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.ByteString
  ( chunkedBy
  , ToByteString(..)
  , ToByteStrings(..)
  , resegment
  , resegmentPadded
  , rechunk
  , hGetContentsChunkedBy
  ) where

import Data.Semigroup ((<>))
import Data.Word

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Vector.Storable     as DVS
import qualified System.IO                as IO
import qualified System.IO.Unsafe         as IO

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

class ToByteStrings a where
  toByteStrings :: a -> [BS.ByteString]

instance ToByteStrings [BS.ByteString] where
  toByteStrings = id
  {-# INLINE toByteStrings #-}

instance ToByteStrings LBS.ByteString where
  toByteStrings = LBS.toChunks
  {-# INLINE toByteStrings #-}

instance ToByteStrings BS.ByteString where
  toByteStrings = (:[])
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word8) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word16) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word32) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word64) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word8] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word16] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word32] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word64] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

-- | Chunk a @bs into list of smaller byte strings of no more than @n elements
chunkedBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs
{-# INLINE chunkedBy #-}

rechunk :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunk size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then let bsNeed = size - bsLen in case bss of
              (cs:css) -> case BS.length cs of
                csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                _                       ->                            go ((bs <> cs)       :css)
              [] -> [bs]
            else if size == bsLen
              then bs:go bss
              else BS.take size bs:go (BS.drop size bs:bss)
          else go bss
        go [] = []

resegment :: Int -> [BS.ByteString] -> [BS.ByteString]
resegment size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then case bss of
              (cs:css) -> let bsNeed = size - bsLen; csLen = BS.length cs in if
                | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                | csLen == bsNeed -> (bs <> cs                ):go                    css
                | otherwise       ->                            go ((bs <> cs)       :css)
              [] -> [bs]
            else let bsCroppedLen = (bsLen `div` size) * size in if bsCroppedLen == bsLen
              then bs:go bss
              else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
          else go bss
        go [] = []

resegmentPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
resegmentPadded size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then let bsNeed = size - bsLen in case bss of
              (cs:css) -> case BS.length cs of
                csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                _                       ->                            go ((bs <> cs)       :css)
              [] -> [bs <> BS.replicate bsNeed 0]
            else case (bsLen `div` size) * size of
              bsCroppedLen -> if bsCroppedLen == bsLen
                then bs:go bss
                else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
          else go bss
        go [] = []

hGetContentsChunkedBy :: Int -> IO.Handle -> IO [BS.ByteString]
hGetContentsChunkedBy k h = lazyRead
  where lazyRead = IO.unsafeInterleaveIO loop
        loop = do
          c <- BSI.createAndTrim k $ \p -> IO.hGetBuf h p k
          if BS.null c
            then IO.hClose h >> return []
            else (c:) <$> lazyRead
