{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Seq
    ( Container(..)
    , Seq(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container
import           HaskellWorks.Data.Positioning

class Container v => Seq v where
  vLength :: v -> Count
  vEnd :: v -> Position
  vEnd = fromIntegral . vLength
  vDrop :: Count -> v -> v
  vTake :: Count -> v -> v
  vUncons :: v -> Maybe (Elem v, v)

instance Seq String where
  vLength = Count . fromIntegral . length
  vEnd = fromIntegral . vLength
  vDrop = drop . fromIntegral
  vTake = take . fromIntegral
  vUncons s = case s of
    (x:xs)  -> Just (x, xs)
    _       -> Nothing

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq BS.ByteString where
  vLength = Count . fromIntegral . BS.length
  vEnd = fromIntegral . vLength
  vDrop = BS.drop . fromIntegral
  vTake = BS.take . fromIntegral
  vUncons = BS.uncons

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word8) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word16) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word32) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word64) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word8) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word16) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word32) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word64) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int8) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int16) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int32) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int64) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int8) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int16) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int32) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int64) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}
