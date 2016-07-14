{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.FromByteString
  ( FromByteString(..)
  ) where

import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.Vector.Storable       as DVS
import           Data.Word

-- | Class for byte-string-like datastructures
class FromByteString a where
  -- | Convert a byte string to a value of type @a
  fromByteString :: BS.ByteString -> a

instance FromByteString (DVS.Vector Word64) where
  fromByteString :: BS.ByteString -> DVS.Vector Word64
  fromByteString bs = DVS.unfoldrN (BS.length bs `div` 8 + 8) gen bs
    where gen :: BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds)  -> gen' 8 (fromIntegral d) ds
            Nothing       -> Nothing
          gen' :: Int -> Word64 -> BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen' n w cs
            | n >= 64   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds)  -> gen' (n + 8) (w .|. (fromIntegral d `shiftL` fromIntegral n)) ds
                Nothing       -> Just (w, cs)
