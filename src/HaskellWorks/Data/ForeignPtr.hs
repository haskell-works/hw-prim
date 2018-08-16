module HaskellWorks.Data.ForeignPtr where

import qualified Foreign.ForeignPtr as F
import qualified System.IO.Unsafe   as IO

allocForeignPtrs :: Int -> IO [F.ForeignPtr a]
allocForeignPtrs size = IO.unsafeInterleaveIO go
  where go = do
          fptr <- F.mallocForeignPtrBytes size
          fptrs <- IO.unsafeInterleaveIO go
          return (fptr:fptrs)
{-# INLINE allocForeignPtrs #-}
