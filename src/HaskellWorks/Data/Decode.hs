{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Decode
  ( Decode(..)
  ) where

class Decode c a where
  decode :: c -> Maybe a
