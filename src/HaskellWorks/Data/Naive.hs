module HaskellWorks.Data.Naive
    ( Naive(..)
    , getNaive
    ) where

newtype Naive a = Naive a deriving (Eq, Show)

getNaive :: Naive a -> a
getNaive (Naive a) = a
{-# INLINE getNaive #-}
