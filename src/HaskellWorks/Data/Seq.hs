{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Seq
    ( Seq(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning

class Seq v where
  type Elem v
  vEmpty :: v
  vLength :: v -> Count
  vEnd :: v -> Position
  vEnd = fromIntegral . vLength
  vDrop :: Count -> v -> v
  vTake :: Count -> v -> v
  vUncons :: v -> Maybe (Elem v, v)

instance Seq String where
  type Elem String = Char
  vEmpty = ""
  vLength = Count . fromIntegral . length
  vEnd = fromIntegral . vLength
  vDrop = drop . fromIntegral
  vTake = take . fromIntegral
  vUncons s = case s of
    (x:xs)  -> Just (x, xs)
    _       -> Nothing

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq BS.ByteString where
  type Elem BS.ByteString = Word8

  vEmpty = BS.empty
  vLength = Count . fromIntegral . BS.length
  vEnd = fromIntegral . vLength
  vDrop = BS.drop . fromIntegral
  vTake = BS.take . fromIntegral
  vUncons = BS.uncons

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int8) where
  type Elem (DV.Vector Int8) = Int8

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int16) where
  type Elem (DV.Vector Int16) = Int16

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int32) where
  type Elem (DV.Vector Int32) = Int32

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DV.Vector Int64) where
  type Elem (DV.Vector Int64) = Int64

  vEmpty = DV.empty
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int8) where
  type Elem (DVS.Vector Int8) = Int8

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int16) where
  type Elem (DVS.Vector Int16) = Int16

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int32) where
  type Elem (DVS.Vector Int32) = Int32

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int64) where
  type Elem (DVS.Vector Int64) = Int64

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}

instance Seq (DVS.Vector Int) where
  type Elem (DVS.Vector Int) = Int

  vEmpty = DVS.empty
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)

  {-# INLINE vEmpty    #-}
  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vUncons   #-}
