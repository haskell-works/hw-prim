{-# LANGUAGE FlexibleInstances #-}
module HaskellWorks.Data.Vector.AsVector64
  ( AsVector64(..)
  ) where

import Data.Word

import qualified Data.Vector.Storable as DVS

class AsVector64 a where
  asVector64 :: a -> DVS.Vector Word64

instance AsVector64 (DVS.Vector Word64) where
  asVector64 = id
