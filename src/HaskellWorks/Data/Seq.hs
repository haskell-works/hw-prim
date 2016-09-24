{-# LANGUAGE FlexibleInstances     #-}
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
  {-# INLINE vEnd #-}

instance Seq String where
  vLength = Count . fromIntegral . length
  {-# INLINE vLength   #-}

instance Seq BS.ByteString where
  vLength = Count . fromIntegral . BS.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Word8) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Word16) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Word32) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Word64) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Word8) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Word16) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Word32) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Word64) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Int8) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Int16) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Int32) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DV.Vector Int64) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Int8) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Int16) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Int32) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Int64) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Seq (DVS.Vector Int) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}
