{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists     #-}

module HaskellWorks.Data.FromByteStringSpec (spec) where

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import           Data.Word
import           HaskellWorks.Data.FromByteString
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.FromByteStringSpec" $ do
  it "fromByteString (BS.unpack []) :: DVS.Vector Word64" $
    let v = fromByteString (BS.pack []) :: DVS.Vector Word64 in
    v `shouldBe` DVS.fromList []
  it "fromByteString (BS.unpack [0x1]) :: DVS.Vector Word64" $
    let v = fromByteString (BS.pack [0x1]) :: DVS.Vector Word64 in
    v `shouldBe` DVS.fromList [0x1]
  it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80]) :: DVS.Vector Word64" $
    let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80]) :: DVS.Vector Word64 in
    v `shouldBe` DVS.fromList [0x8040201008040201]
  it "fromByteString (BS.unpack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x81]) :: DVS.Vector Word64" $
    let v = fromByteString (BS.pack [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x81]) :: DVS.Vector Word64 in
    v `shouldBe` DVS.fromList [0x8040201008040201, 0x81]
