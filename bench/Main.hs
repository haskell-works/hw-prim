{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Word
import Foreign
import HaskellWorks.Data.FromForeignRegion

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS
import qualified System.IO                as IO
import qualified System.IO.MMap           as IO

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
  IO.hPutStrLn IO.stderr $ "Length ByteString: " <> show (BS.length bs)
  let !_ = BS.foldl' (+) 0 bs
  return ()

sumFileVectorWord64 :: FilePath -> IO ()
sumFileVectorWord64 filePath = do
  !(v :: DVS.Vector Word64) <- mmapVectorLike filePath
  IO.hPutStrLn IO.stderr $ "Length Vector: " <> show (DVS.length v)
  let !_ = DVS.foldl' (+) 0 v
  return ()

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (return ()) $ \_ -> bgroup "medium.csv"
    [ bench "Foldl' over ByteString"    (whnfIO (sumFileByteString   "corpus/medium.csv"))
    , bench "Foldl' over Vector Word64" (whnfIO (sumFileVectorWord64 "corpus/medium.csv"))
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40Conduits
