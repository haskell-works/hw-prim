{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.Storable
  ( padded
  , foldMap
  , mapAccumL
  , mapAccumLViaLazyState
  , mapAccumLViaStrictState
  ) where

import Control.Monad.ST     (ST)
import Data.Monoid          (Monoid (..), (<>))
import Data.Tuple
import Data.Vector.Storable (Storable)
import Data.Word
import Prelude              hiding (foldMap)

import qualified Control.Monad.State.Lazy     as MSL
import qualified Control.Monad.State.Strict   as MSS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

padded :: Int -> DVS.Vector Word8 -> DVS.Vector Word8
padded n v = v <> DVS.replicate ((n - DVS.length v) `max` 0) 0
{-# INLINE padded #-}

foldMap :: (DVS.Storable a, Monoid m) => (a -> m) -> DVS.Vector a -> m
foldMap f = DVS.foldl' (\a b -> a <> f b) mempty
{-# INLINE foldMap #-}

mapAccumL :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumL f a vb = DVS.createT $ do
  vc <- DVSM.unsafeNew (DVS.length vb)
  a' <- go 0 a vc
  return (a', vc)
  where go :: Int -> a -> DVS.MVector s c -> ST s a
        go i a0 vc = if i < DVS.length vb
          then do
            let (a1, c1) = f a0 (DVS.unsafeIndex vb i)
            DVSM.unsafeWrite vc i c1
            go (i + 1) a1 vc
          else return a0
{-# INLINE mapAccumL #-}

mapAccumLViaLazyState :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumLViaLazyState f a vb = swap . flip MSL.runState a $ DVS.forM vb go
  where go b = do
          a0 <- MSL.get
          let (a1, c1) = f a0 b
          MSL.put a1
          return c1
{-# INLINE mapAccumLViaLazyState #-}

mapAccumLViaStrictState :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumLViaStrictState f a vb = swap . flip MSS.runState a $ DVS.forM vb go
  where go b = do
          a0 <- MSS.get
          let (a1, c1) = f a0 b
          MSS.put a1
          return c1
{-# INLINE mapAccumLViaStrictState #-}
