{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.ByteString
  ( chunkedBy
  , ToByteString(..)
  , ToByteStrings(..)
  , resegment
  , resegmentPadded
  , rechunk
  , rechunkPadded
  , hGetContentsChunkedBy
  ) where

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

resegment :: Int -> [BS.ByteString] -> [BS.ByteString]
resegment size = repartition $ begin
  where begin :: [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        begin ass@(bs:bss) = if BS.length bs >= size
          then let (ds, es) = BS.splitAt ((BS.length bs `div` size) * size) bs in ([ds], es:bss)
          else go size ass
        begin [] = ([], [])
        go :: Int -> [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        go n (bs:bss) = let len = BS.length bs in if len >= n
          then let (ds , es ) = BS.splitAt n bs           in ([ds]  , es:bss)
          else let (css, dss) = go (n - BS.length bs) bss in (bs:css,    dss)
        go _ []       = ([], [])

rechunk :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunk size = repartition (go size)
  where go :: Int -> [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        go n bss =
          case bss of
          cs:css | BS.length cs == 0 -> go n css
          cs:css | BS.length cs >= n -> let (ds , es ) = BS.splitAt n cs           in ([ds]  , es:css)
          cs:css                     -> let (dss, ess) = go (n - BS.length cs) css in (cs:dss,    ess)
          []                         -> ([]                 , [])

rechunkPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkPadded size = repartition (go size)
  where go :: Int -> [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        go n bss =
          case bss of
          cs:css | BS.length cs == 0 -> go n css
          cs:css | BS.length cs >= n -> let (ds , es ) = BS.splitAt n cs           in ([ds]  , es:css)
          cs:css                     -> let (dss, ess) = go (n - BS.length cs) css in (cs:dss,    ess)
          [] | n < size              -> ([BS.replicate n 0] , [])
          []                         -> ([]                 , [])

resegmentPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
resegmentPadded size = repartition $ begin
  where begin :: [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        begin ass@(bs:bss) = if BS.length bs >= size
          then let (ds, es) = BS.splitAt ((BS.length bs `div` size) * size) bs in ([ds], es:bss)
          else go size ass
        begin [] = ([], [])
        go :: Int -> [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
        go n (bs:bss) = let len = BS.length bs in if len >= n
          then let (ds , es ) = BS.splitAt n bs           in ([ds]  , es:bss)
          else let (css, dss) = go (n - BS.length bs) bss in (bs:css,    dss)
        go n []       = ([BS.replicate n 0], [])

repartition :: ([BS.ByteString] -> ([BS.ByteString], [BS.ByteString])) -> [BS.ByteString] -> [BS.ByteString]
repartition f = go
  where go []  = []
        go bss@(cs:css) = if BS.length cs > 0
          then let (dss, ess) = f bss in BS.concat dss:go ess
          else go css

hGetContentsChunkedBy :: Int -> IO.Handle -> IO [BS.ByteString]
hGetContentsChunkedBy k h = lazyRead
  where lazyRead = IO.unsafeInterleaveIO loop
        loop = do
            c <- BSI.createAndTrim k $ \p -> IO.hGetBuf h p k
            if BS.null c
              then IO.hClose h >> return []
              else (c:) <$> lazyRead
