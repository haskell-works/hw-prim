module HaskellWorks.Data.Char.IsChar
    ( IsChar(..)
    ) where

import qualified Data.ByteString.Internal as BI
import           Data.Word

class IsChar c where
  toChar :: c -> Char

instance IsChar Word8 where
  toChar = BI.w2c
  {-# INLINABLE toChar #-}

instance IsChar Char where
  toChar = id
  {-# INLINABLE toChar #-}
