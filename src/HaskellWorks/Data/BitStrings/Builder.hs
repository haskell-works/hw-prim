{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.BitStrings.Builder where

import GHC.Generics
import HaskellWorks.Data.BitStrings

newtype BitStringBuilder = BitStringBuilder
  { builder :: BitStrings -> BitStrings
  } deriving Generic

instance Semigroup BitStringBuilder where
  BitStringBuilder a <> BitStringBuilder b = BitStringBuilder (a <> b)

instance Monoid BitStringBuilder where
  mempty = BitStringBuilder mempty
