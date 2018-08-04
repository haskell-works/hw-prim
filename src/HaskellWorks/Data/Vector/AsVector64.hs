{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Vector.AsVector64
  ( AsVector64(..)
  ) where

import Data.Bits
import Data.Word

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

class AsVector64 a where
  asVector64 :: a -> DVS.Vector Word64

instance AsVector64 (DVS.Vector Word64) where
  asVector64 = id

instance AsVector64 BS.ByteString where
  asVector64 bs = DVS.constructN ((BS.length bs + 7) `div` 8) go
    where go :: DVS.Vector Word64 -> Word64
          go u = let bsi = DVS.length u * 8 in
            if bsi + 8 <= BS.length bs
              then  let w0 = fromIntegral $ BS.index bs (bsi + 0)
                        w1 = fromIntegral $ BS.index bs (bsi + 1)
                        w2 = fromIntegral $ BS.index bs (bsi + 2)
                        w3 = fromIntegral $ BS.index bs (bsi + 3)
                        w4 = fromIntegral $ BS.index bs (bsi + 4)
                        w5 = fromIntegral $ BS.index bs (bsi + 5)
                        w6 = fromIntegral $ BS.index bs (bsi + 6)
                        w7 = fromIntegral $ BS.index bs (bsi + 7)
                    in  (w0 `shiftL`  0) .|.
                        (w1 `shiftL`  8) .|.
                        (w2 `shiftL` 16) .|.
                        (w3 `shiftL` 24) .|.
                        (w4 `shiftL` 32) .|.
                        (w5 `shiftL` 40) .|.
                        (w6 `shiftL` 48) .|.
                        (w7 `shiftL` 56)
              else  let w0 = let i = bsi + 0 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 0) else 0
                        w1 = let i = bsi + 1 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 1) else 0
                        w2 = let i = bsi + 2 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 2) else 0
                        w3 = let i = bsi + 3 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 3) else 0
                        w4 = let i = bsi + 4 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 4) else 0
                        w5 = let i = bsi + 5 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 5) else 0
                        w6 = let i = bsi + 6 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 6) else 0
                        w7 = let i = bsi + 7 in if i < BS.length bs then fromIntegral $ BS.index bs (bsi + 7) else 0
                    in  (w0 `shiftL`  0) .|.
                        (w1 `shiftL`  8) .|.
                        (w2 `shiftL` 16) .|.
                        (w3 `shiftL` 24) .|.
                        (w4 `shiftL` 32) .|.
                        (w5 `shiftL` 40) .|.
                        (w6 `shiftL` 48) .|.
                        (w7 `shiftL` 56)

