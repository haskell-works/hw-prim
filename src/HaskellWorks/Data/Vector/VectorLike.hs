{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Vector.VectorLike
  ( Container(..)
  , IndexedSeq(..)
  , Seq(..)
  , VectorLike(..)
  ) where

import qualified Data.ByteString               as BS
import           Data.Int
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.IndexedSeq
import           HaskellWorks.Data.Snoc

-- | Class of values that support vector like operations
class (IndexedSeq v, Snoc v) => VectorLike v where
  vConcat :: [v] -> v
  vFilter :: (Elem v -> Bool) -> v -> v
  vGenerate :: Int -> (Int -> Elem v) -> v

instance VectorLike String where
  vConcat = concat
  vFilter = filter
  vGenerate n f = f `fmap` [0 .. (n - 1)]
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike BS.ByteString where
  vConcat = BS.concat
  vFilter = BS.filter
  vGenerate n f = fst (BS.unfoldrN n go 0)
    where go i = if i /= n then Just (f i, i + 1) else Nothing
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Word8) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Word16) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Word32) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Word64) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Word8) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Word16) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Word32) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Word64) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Int8) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Int16) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Int32) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DV.Vector Int64) where
  vConcat = DV.concat
  vFilter = DV.filter
  vGenerate = DV.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Int8) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Int16) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Int32) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Int64) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}

instance VectorLike (DVS.Vector Int) where
  vConcat = DVS.concat
  vFilter = DVS.filter
  vGenerate = DVS.generate
  {-# INLINE vConcat   #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
