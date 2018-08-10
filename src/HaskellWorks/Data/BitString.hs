{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.BitString where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.Positioning
import Prelude                       hiding (length)

import qualified Data.Vector.Storable     as DVS
import qualified HaskellWorks.Data.Length as HW

data BitString = BitString
  { vector :: !(DVS.Vector Word64)
  , offset :: !Position
  , length :: !Count
  } deriving (Generic)

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id

instance ToBitString (DVS.Vector Word64) where
  toBitString v = BitString
    { vector = v
    , offset = 0
    , length = HW.length v * 64
    }
