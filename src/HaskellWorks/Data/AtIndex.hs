{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.AtIndex
    ( Container(..)
    , AtIndex(..)
    , Length(..)
    , atIndexOr
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Length
import HaskellWorks.Data.Positioning

import qualified Data.ByteString          as BS
import qualified Data.Vector              as DV
import qualified Data.Vector.Storable     as DVS
import qualified HaskellWorks.Data.Length as HW

class Length v => AtIndex v where
  (!!!)     :: v -> Position -> Elem v
  atIndex   :: v -> Position -> Elem v

instance AtIndex [a] where
  (!!!)   v i = v !! fromIntegral i
  atIndex v i = v !! fromIntegral i
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex BS.ByteString where
  (!!!)   v i = v `BS.index` fromIntegral i
  atIndex v i = BS.index v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word8) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word16) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word32) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word64) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word8) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word16) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word32) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word64) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int8) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int16) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int32) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int64) where
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int8) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int16) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int32) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int64) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int) where
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

atIndexOr :: AtIndex v => Elem v -> v -> Position -> Elem v
atIndexOr d v p = if p < fromIntegral (HW.length v) then v !!! p else d
{-# INLINE atIndexOr #-}
