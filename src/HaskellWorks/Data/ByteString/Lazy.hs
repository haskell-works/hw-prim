{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.ByteString.Lazy where

import Data.Word
import HaskellWorks.Data.ByteString (ToByteString (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

class ToLazyByteString a where
  toLazyByteString :: a -> LBS.ByteString

instance ToLazyByteString LBS.ByteString where
  toLazyByteString = id
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word8) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word16) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word32) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString (DVS.Vector Word64) where
  toLazyByteString = LBS.fromStrict . toByteString
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word8] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word16] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word32] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}

instance ToLazyByteString [DVS.Vector Word64] where
  toLazyByteString vs = LBS.fromChunks (toByteString <$> vs)
  {-# INLINE toLazyByteString #-}
