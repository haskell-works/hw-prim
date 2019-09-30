module HaskellWorks.Foreign
  ( mallocForeignPtrBytesWithAlignedPtr
  ) where

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr        (Ptr, alignPtr)
import Foreign.Storable   (Storable)

import qualified Foreign.ForeignPtr        as F
import qualified Foreign.ForeignPtr.Unsafe as F

mallocForeignPtrBytesWithAlignedPtr :: Storable a => Int -> Int -> IO (ForeignPtr a, Ptr a)
mallocForeignPtrBytesWithAlignedPtr alignment n = do
  fptr <- F.mallocForeignPtrBytes (n + alignment)
  let alignedPtr = alignPtr (F.unsafeForeignPtrToPtr fptr) alignment
  return (fptr, alignedPtr)
