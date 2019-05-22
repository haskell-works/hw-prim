{-# LANGUAGE MagicHash #-}

module HaskellWorks.Data.Branchless
  ( ltWord8
  , ltWord16
  , ltWord32
  , ltWord64

  , leWord8
  , leWord16
  , leWord32
  , leWord64

  , gtWord8
  , gtWord16
  , gtWord32
  , gtWord64

  , geWord8
  , geWord16
  , geWord32
  , geWord64
  ) where

import GHC.Int
import GHC.Prim
import GHC.Word (Word16 (..), Word32 (..), Word64 (..), Word8 (..))

ltWord8 :: Word8 -> Word8 -> Word8
ltWord8 (W8# a#) (W8# b#) = fromIntegral (I8# (ltWord# a# b#))
{-# INLINE ltWord8 #-}

ltWord16 :: Word16 -> Word16 -> Word16
ltWord16 (W16# a#) (W16# b#) = fromIntegral (I16# (ltWord# a# b#))
{-# INLINE ltWord16 #-}

ltWord32 :: Word32 -> Word32 -> Word32
ltWord32 (W32# a#) (W32# b#) = fromIntegral (I32# (ltWord# a# b#))
{-# INLINE ltWord32 #-}

ltWord64 :: Word64 -> Word64 -> Word64
ltWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord64 #-}

leWord8 :: Word8 -> Word8 -> Word8
leWord8 (W8# a#) (W8# b#) = fromIntegral (I8# (leWord# a# b#))
{-# INLINE leWord8 #-}

leWord16 :: Word16 -> Word16 -> Word16
leWord16 (W16# a#) (W16# b#) = fromIntegral (I16# (leWord# a# b#))
{-# INLINE leWord16 #-}

leWord32 :: Word32 -> Word32 -> Word32
leWord32 (W32# a#) (W32# b#) = fromIntegral (I32# (leWord# a# b#))
{-# INLINE leWord32 #-}

leWord64 :: Word64 -> Word64 -> Word64
leWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (leWord# a# b#))
{-# INLINE leWord64 #-}

gtWord8 :: Word8 -> Word8 -> Word8
gtWord8 (W8# a#) (W8# b#) = fromIntegral (I8# (gtWord# a# b#))
{-# INLINE gtWord8 #-}

gtWord16 :: Word16 -> Word16 -> Word16
gtWord16 (W16# a#) (W16# b#) = fromIntegral (I16# (gtWord# a# b#))
{-# INLINE gtWord16 #-}

gtWord32 :: Word32 -> Word32 -> Word32
gtWord32 (W32# a#) (W32# b#) = fromIntegral (I32# (gtWord# a# b#))
{-# INLINE gtWord32 #-}

gtWord64 :: Word64 -> Word64 -> Word64
gtWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (gtWord# a# b#))
{-# INLINE gtWord64 #-}

geWord8 :: Word8 -> Word8 -> Word8
geWord8 (W8# a#) (W8# b#) = fromIntegral (I8# (geWord# a# b#))
{-# INLINE geWord8 #-}

geWord16 :: Word16 -> Word16 -> Word16
geWord16 (W16# a#) (W16# b#) = fromIntegral (I16# (geWord# a# b#))
{-# INLINE geWord16 #-}

geWord32 :: Word32 -> Word32 -> Word32
geWord32 (W32# a#) (W32# b#) = fromIntegral (I32# (geWord# a# b#))
{-# INLINE geWord32 #-}

geWord64 :: Word64 -> Word64 -> Word64
geWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (geWord# a# b#))
{-# INLINE geWord64 #-}
