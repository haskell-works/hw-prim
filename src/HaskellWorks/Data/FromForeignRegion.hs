{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.FromForeignRegion where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.FromByteString

type ForeignRegion = (ForeignPtr Word8, Int, Int)

-- | Class for datastructures that can be created from a foreign region
class FromForeignRegion a where
  -- | Create a value of type @a from a foreign region.
  fromForeignRegion :: ForeignRegion -> a

instance FromForeignRegion BS.ByteString where
  fromForeignRegion (fptr, offset, size) = BSI.fromForeignPtr (castForeignPtr fptr) offset size

instance FromForeignRegion (DVS.Vector Word8) where
  fromForeignRegion = fromByteString . fromForeignRegion

instance FromForeignRegion (DVS.Vector Word16) where
  fromForeignRegion = fromByteString . fromForeignRegion

instance FromForeignRegion (DVS.Vector Word32) where
  fromForeignRegion = fromByteString . fromForeignRegion

instance FromForeignRegion (DVS.Vector Word64) where
  fromForeignRegion = fromByteString . fromForeignRegion
