{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Length
    ( Container(..)
    , Length(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container
import           HaskellWorks.Data.Positioning

class Container v => Length v where
  vLength :: v -> Count
  vEnd :: v -> Position
  vEnd = fromIntegral . vLength
  {-# INLINE vEnd #-}

instance Length String where
  vLength = Count . fromIntegral . length
  {-# INLINE vLength   #-}

instance Length BS.ByteString where
  vLength = Count . fromIntegral . BS.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Word8) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Word16) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Word32) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Word64) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Word8) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Word16) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Word32) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Word64) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Int8) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Int16) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Int32) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DV.Vector Int64) where
  vLength = Count . fromIntegral . DV.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Int8) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Int16) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Int32) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Int64) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}

instance Length (DVS.Vector Int) where
  vLength = Count . fromIntegral . DVS.length
  {-# INLINE vLength   #-}
