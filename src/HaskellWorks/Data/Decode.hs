{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Decode
  ( Decode(..)
  , DecodeError(..)
  ) where

data DecodeError = DecodeError String deriving (Eq, Show)

class Decode s t where
  decode :: s -> Either DecodeError t
