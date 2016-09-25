{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Positioning
  ( Count(..)
  , Position(..)
  , lastPositionOf
  , toCount
  , toPosition
  ) where

import           Control.DeepSeq
import           Data.Int
import           Data.Word
import           System.Random

-- | A value representing a count
newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Integral, Real, Random)

instance Show Count where
    show (Count w64) = show w64

    -- | A value representing a position
newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Position where
    show (Position n) = show n

instance NFData Count where
  rnf = rnf . getCount
  {-# INLINE rnf #-}

instance NFData Position where
  rnf = rnf . getPosition
  {-# INLINE rnf #-}

class ToPosition a where
  -- | Convert to a position
  toPosition :: a -> Position

instance ToPosition Count where
  toPosition (Count n) = Position (fromIntegral n)
  {-# INLINE toPosition #-}

class ToCount a where
  -- | Convert a count to a count
  toCount :: a -> Count

instance ToCount Position where
  toCount (Position n) = Count (fromIntegral n)
  {-# INLINE toCount #-}

-- | Get largest position in a sequenced container of size count.
lastPositionOf :: Count -> Position
lastPositionOf (Count c)  = Position (fromIntegral c - 1)
