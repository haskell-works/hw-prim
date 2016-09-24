{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Snoc
    ( Snoc(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Seq

class Seq v => Snoc v where
  vSnoc :: v -> Elem v -> v

instance Snoc String where
  vSnoc v c = v ++ [c]
  {-# INLINE vSnoc     #-}

instance Snoc BS.ByteString where
  vSnoc = BS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Word8) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Word16) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Word32) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Word64) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Word8) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Word16) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Word32) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Word64) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Int8) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Int16) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Int32) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DV.Vector Int64) where
  vSnoc = DV.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Int8) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Int16) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Int32) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Int64) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}

instance Snoc (DVS.Vector Int) where
  vSnoc = DVS.snoc
  {-# INLINE vSnoc     #-}
