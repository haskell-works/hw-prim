{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.SearchSpec (spec) where

import           HaskellWorks.Data.Search
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SearchSpec" $ do
  it "No tests" $ do
    let xs :: [Int] = [0, 256, 512, 768, 1024, 1280, 1536, 1792, 2048]
    binarySearch 513 (xs !!) 2 4 `shouldBe` 2
