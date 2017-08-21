{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Filter
  ( Container(..)
  , Filter(..)
  ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.List                     as L
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container

class Container v => Filter v where
  -- | Filter the element of a container with the given predicate
  filter :: (Elem v -> Bool) -> v -> v

instance Filter String where
  filter = L.filter
  {-# INLINE filter #-}

instance Filter BS.ByteString where
  filter = BS.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Word8) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Word16) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Word32) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Word64) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Word8) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Word16) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Word32) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Word64) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Int8) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Int16) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Int32) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DV.Vector Int64) where
  filter = DV.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Int8) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Int16) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Int32) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Int64) where
  filter = DVS.filter
  {-# INLINE filter #-}

instance Filter (DVS.Vector Int) where
  filter = DVS.filter
  {-# INLINE filter #-}
