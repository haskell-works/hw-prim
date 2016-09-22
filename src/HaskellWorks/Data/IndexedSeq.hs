{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.IndexedSeq
    ( IndexedSeq(..)
    , Seq(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Seq

class Seq v => IndexedSeq v where
  (!!!)   :: v -> Position -> Elem v
  vIndex  :: v -> Position -> Elem v

instance IndexedSeq String where
  (!!!)   v (Position i) = v !! fromIntegral i
  vIndex  v (Position i) = v !! fromIntegral i
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq BS.ByteString where
  (!!!)   v (Position i) = v `BS.index` fromIntegral i
  vIndex  v (Position i) = BS.index v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Word8) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Word16) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Word32) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Word64) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Word8) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Word16) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Word32) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Word64) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Int8) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Int16) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Int32) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DV.Vector Int64) where
  (!!!)   v (Position i) = v DV.! fromIntegral i
  vIndex  v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Int8) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Int16) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Int32) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Int64) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}

instance IndexedSeq (DVS.Vector Int) where
  (!!!)   v (Position i) = v DVS.! fromIntegral i
  vIndex  v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)     #-}
  {-# INLINE vIndex    #-}
