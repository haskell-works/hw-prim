{-# LANGUAGE MagicHash #-}

module HaskellWorks.Data.Word
  ( ltWord64
  ) where

import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Word  hiding (ltWord64)

ltWord64 :: Word64 -> Word64 -> Word64
ltWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord64 #-}
