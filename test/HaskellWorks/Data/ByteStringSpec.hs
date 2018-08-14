module HaskellWorks.Data.ByteStringSpec where

import Control.Monad
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified HaskellWorks.Data.ByteString as BS
import qualified Hedgehog.Gen                 as G
import qualified Hedgehog.Range               as R

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.ByteStringSpec" $ do
  it "rechunkSegments does not modify data" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    mconcat (BS.rechunkSegments chunkSize bss) === mconcat bss
  it "rechunkSegments creates correctly sized segments" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    forM_ (drop 1 (reverse (BS.rechunkSegments chunkSize bss))) $ \bs -> do
      (BS.length bs) `mod` chunkSize === 0
  it "rechunkSegmentsPadded does not modify data" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    elbs      <- forAll $ pure $ LBS.fromChunks bss
    albs      <- forAll $ pure $ LBS.take (LBS.length elbs) (LBS.fromChunks (BS.rechunkSegmentsPadded chunkSize bss))
    albs === elbs
  it "rechunkSegmentsPadded creates correctly sized segments" $ requireProperty $ do
    bss       <- forAll $ (BS.pack <$>) <$> G.list (R.linear 0 8) (G.list (R.linear 0 24) (G.word8 R.constantBounded))
    chunkSize <- forAll $ G.int (R.linear 1 4)
    forM_ (reverse (BS.rechunkSegmentsPadded chunkSize bss)) $ \bs -> do
      (BS.length bs) `mod` chunkSize === 0

