{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Drop
    ( Container(..)
    , Drop(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container
import           HaskellWorks.Data.Positioning

class Container v => Drop v where
  vDrop :: Count -> v -> v

instance Drop String where
  vDrop = drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop BS.ByteString where
  vDrop = BS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Word8) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Word16) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Word32) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Word64) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Word8) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Word16) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Word32) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Word64) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Int8) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Int16) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Int32) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DV.Vector Int64) where
  vDrop = DV.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Int8) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Int16) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Int32) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Int64) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}

instance Drop (DVS.Vector Int) where
  vDrop = DVS.drop . fromIntegral
  {-# INLINE vDrop #-}
