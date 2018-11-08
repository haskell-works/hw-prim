{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Word
import Foreign
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Vector.AsVector64s

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Internal          as BSI
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Vector.Storable as DVS
import qualified System.IO.MMap                    as IO

setupEnvByteString :: FilePath -> IO BS.ByteString
setupEnvByteString filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filepath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

mmapVectorLike :: FromForeignRegion a => FilePath -> IO a
mmapVectorLike filePath = do
  region <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = fromForeignRegion region
  return bs

sumFileByteString :: FilePath -> IO ()
sumFileByteString filePath = do
  !(bs :: BS.ByteString) <- mmapVectorLike filePath
  let !_ = BS.foldl' (+) 0 bs
  return ()

sumFileVector64 :: FilePath -> IO ()
sumFileVector64 filePath = do
  !(v :: DVS.Vector Word64) <- mmapVectorLike filePath
  let !_ = DVS.foldl' (+) 0 v
  return ()

sumFileLazyByteStringViaVector64 :: Int -> FilePath -> IO ()
sumFileLazyByteStringViaVector64 multiple filePath = do
  !bs <- LBS.readFile filePath
  let wss = asVector64s multiple bs
  let !_ = sum (DVS.foldl' (+) 0 <$> wss)
  return ()

mkBenchMapAccumL :: (Word64 -> Word64 -> (Word64, Word64)) -> Word64 -> FilePath -> IO ()
mkBenchMapAccumL f s filePath = do
  !(v :: DVS.Vector Word64) <- mmapVectorLike filePath
  let !_ = snd $ DVS.mapAccumL f s v
  return ()

mkBenchMapAccumLViaStrictState :: (Word64 -> Word64 -> (Word64, Word64)) -> Word64 -> FilePath -> IO ()
mkBenchMapAccumLViaStrictState f s filePath = do
  !(v :: DVS.Vector Word64) <- mmapVectorLike filePath
  let !_ = snd $ DVS.mapAccumLViaStrictState f s v
  return ()

mkBenchMapAccumLViaLazyState :: (Word64 -> Word64 -> (Word64, Word64)) -> Word64 -> FilePath -> IO ()
mkBenchMapAccumLViaLazyState f s filePath = do
  !(v :: DVS.Vector Word64) <- mmapVectorLike filePath
  let !_ = snd $ DVS.mapAccumLViaLazyState f s v
  return ()

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (return ()) $ \_ -> bgroup "medium.csv"
    -- [ bench "Foldl' over ByteString"                        (whnfIO (sumFileByteString                "corpus/medium.csv"))
    -- , bench "Foldl' over Vector Word64"                     (whnfIO (sumFileVector64                  "corpus/medium.csv"))
    -- [ bench "Foldl' over Lazy ByteString via Vector Word64" (whnfIO (sumFileLazyByteStringViaVector64 512 "corpus/medium.csv"))
    -- , bench "Foldl' over Lazy ByteString via Vector Word64" (whnfIO (sumFileLazyByteStringViaVector64 256 "corpus/medium.csv"))
    -- , bench "Foldl' over Lazy ByteString via Vector Word64" (whnfIO (sumFileLazyByteStringViaVector64 128 "corpus/medium.csv"))
    -- , bench "Foldl' over Lazy ByteString via Vector Word64" (whnfIO (sumFileLazyByteStringViaVector64 64  "corpus/medium.csv"))
    [ bench "mapAccumL               for DVS.Vector Word64" (whnfIO (mkBenchMapAccumL               (\a b -> (a + b, a * b)) 3 "corpus/small.csv"))
    , bench "mapAccumLViaStrictState for DVS.Vector Word64" (whnfIO (mkBenchMapAccumLViaStrictState (\a b -> (a + b, a * b)) 3 "corpus/small.csv"))
    , bench "mapAccumLViaLazyState   for DVS.Vector Word64" (whnfIO (mkBenchMapAccumLViaLazyState   (\a b -> (a + b, a * b)) 3 "corpus/small.csv"))
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40Conduits
