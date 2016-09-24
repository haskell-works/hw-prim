{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Uncons
    ( Container(..)
    , Uncons(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Container
import           HaskellWorks.Data.Drop

class Drop v => Uncons v where
  vUncons :: v -> Maybe (Elem v, v)

instance Uncons String where
  vUncons s = case s of
    (x:xs)  -> Just (x, xs)
    _       -> Nothing
  {-# INLINE vUncons   #-}

instance Uncons BS.ByteString where
  vUncons = BS.uncons
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Word8) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Word16) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Word32) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Word64) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Word8) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Word16) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Word32) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Word64) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Int8) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Int16) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Int32) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DV.Vector Int64) where
  vUncons s = if DV.length s == 0 then Nothing else Just (DV.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Int8) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Int16) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Int32) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Int64) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}

instance Uncons (DVS.Vector Int) where
  vUncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, vDrop 1 s)
  {-# INLINE vUncons   #-}
