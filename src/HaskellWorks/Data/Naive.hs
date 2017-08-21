module HaskellWorks.Data.Naive
    ( Naive(..)
    , naive
    ) where

-- | Data type providing reference implementations of typeclasses.  Such
-- implementations may be inefficient.
newtype Naive a = Naive a deriving (Eq, Show)

naive :: Naive a -> a
naive (Naive a) = a
{-# INLINE naive #-}
