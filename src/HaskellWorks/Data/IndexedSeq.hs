{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies        #-}

module HaskellWorks.Data.IndexedSeq
    ( Container(..)
    , IndexedSeq(..)
    , Length(..)
    ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Length
import           HaskellWorks.Data.Positioning

class Length v => IndexedSeq v where
  (!!!) :: v -> Position -> Elem v
  index :: v -> Position -> Elem v

instance IndexedSeq [a] where
  (!!!) v (Position i) = v !! fromIntegral i
  index v (Position i) = v !! fromIntegral i
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq BS.ByteString where
  (!!!) v (Position i) = v `BS.index` fromIntegral i
  index v (Position i) = BS.index v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Word8) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Word16) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Word32) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Word64) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Word8) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Word16) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Word32) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Word64) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Int8) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Int16) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Int32) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DV.Vector Int64) where
  (!!!) v (Position i) = v DV.! fromIntegral i
  index v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Int8) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Int16) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Int32) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Int64) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}

instance IndexedSeq (DVS.Vector Int) where
  (!!!) v (Position i) = v DVS.! fromIntegral i
  index v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINE (!!!) #-}
  {-# INLINE index #-}
