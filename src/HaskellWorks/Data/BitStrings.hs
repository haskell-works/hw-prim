{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.BitStrings where

import Data.Semigroup              (Semigroup (..))
import GHC.Generics
import HaskellWorks.Data.BitString

newtype BitStrings = BitStrings
  { chunks :: [BitString]
  } deriving (Generic)

instance Semigroup BitStrings where
  BitStrings as <> BitStrings bs = BitStrings (as <> bs)

instance Monoid BitStrings where
  mempty = BitStrings mempty

class ToBitStrings a where
  toBitStrings :: a -> BitStrings

instance ToBitStrings BitStrings where
  toBitStrings = id
  {-# INLINE toBitStrings #-}

instance ToBitStrings BitString where
  toBitStrings bs = BitStrings [bs]
  {-# INLINE toBitStrings #-}
