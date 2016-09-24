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
  vTake :: Count -> v -> v

instance Seq String where
  vLength = Count . fromIntegral . length
  vEnd = fromIntegral . vLength
  vTake = take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq BS.ByteString where
  vLength = Count . fromIntegral . BS.length
  vEnd = fromIntegral . vLength
  vTake = BS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Word8) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Word16) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Word32) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Word64) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Word8) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Word16) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Word32) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Word64) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Int8) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Int16) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Int32) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DV.Vector Int64) where
  vLength = Count . fromIntegral . DV.length
  vEnd = fromIntegral . vLength
  vTake = DV.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Int8) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Int16) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Int32) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Int64) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}

instance Seq (DVS.Vector Int) where
  vLength = Count . fromIntegral . DVS.length
  vEnd = fromIntegral . vLength
  vTake = DVS.take . fromIntegral

  {-# INLINE vLength   #-}
  {-# INLINE vEnd      #-}
  {-# INLINE vTake     #-}
