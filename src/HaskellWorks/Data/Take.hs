{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Take
    ( Container(..)
    , Take(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container
import           HaskellWorks.Data.Positioning

class Container v => Take v where
  vTake :: Count -> v -> v

instance Take String where
  vTake = take . fromIntegral
  {-# INLINE vTake #-}

instance Take BS.ByteString where
  vTake = BS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Word8) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Word16) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Word32) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Word64) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Word8) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Word16) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Word32) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Word64) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Int8) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Int16) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Int32) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DV.Vector Int64) where
  vTake = DV.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Int8) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Int16) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Int32) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Int64) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}

instance Take (DVS.Vector Int) where
  vTake = DVS.take . fromIntegral
  {-# INLINE vTake #-}
