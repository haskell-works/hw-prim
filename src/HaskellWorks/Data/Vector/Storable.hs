{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.Storable
  ( padded
  , foldMap
  , mapAccumL
  , mapAccumLViaLazyState
  , mapAccumLViaStrictState
  , mapAccumLFusable
  ) where

import Control.Monad
import Control.Monad.ST          (ST)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid               (Monoid (..), (<>))
import Data.Tuple
import Data.Vector.Fusion.Bundle (Bundle, MBundle, inplace)
import Data.Vector.Storable      (Storable)
import Data.Word
import Prelude                   hiding (foldMap)

import qualified Control.Monad.State.Lazy          as MSL
import qualified Control.Monad.State.Strict        as MSS
import qualified Data.Vector.Fusion.Bundle         as DVFB
import qualified Data.Vector.Fusion.Bundle.Monadic as DVFBM
import qualified Data.Vector.Fusion.Stream.Monadic as DVFSM
import qualified Data.Vector.Fusion.Util           as DVFU
import qualified Data.Vector.Generic               as DVG
import qualified Data.Vector.Storable              as DVS
import qualified Data.Vector.Storable.Mutable      as DVSM

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

unstreamM :: (Storable a, Monad m) => MBundle m u a -> m (DVS.Vector a)
{-# INLINE [1] unstreamM #-}
unstreamM s = do
  xs <- DVFBM.toList s
  return $ DVG.unstream $ DVFB.unsafeFromList (DVFBM.size s) xs

mapAccumLFusable :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumLFusable f a v = swap $ DVFU.unId $ flip runStateT a $ unstreamM (bundleMapAccumL f a (DVG.stream v))
{-# INLINE mapAccumLFusable #-}

-- | Map a function over a 'Bundle'
bundleMapAccumL :: Monad m => (a -> b -> (a, c)) -> a -> DVFBM.Bundle m v b -> DVFBM.Bundle (StateT a m) v c
bundleMapAccumL f = bundleMapAccumLM (\a b -> return (f a b))
{-# INLINE bundleMapAccumL #-}

-- | Map a monadic function over a 'Bundle'
bundleMapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> DVFBM.Bundle m v b -> DVFBM.Bundle (StateT a m) v c
bundleMapAccumLM f a DVFBM.Bundle{DVFBM.sElems = s, DVFBM.sSize = n} = DVFBM.fromStream (streamMapAccumLM f a s) n
{-# INLINE_FUSED bundleMapAccumLM #-}

streamMapAccumL :: Monad m => (a -> b -> (a, c)) -> a -> DVFSM.Stream m b -> DVFSM.Stream (StateT a m) c
streamMapAccumL f = streamMapAccumLM (\a b -> return (f a b))
{-# INLINE streamMapAccumL #-}

streamMapAccumLM :: forall m a b c . Monad m => (a -> b -> m (a, c)) -> a -> DVFSM.Stream m b -> DVFSM.Stream (StateT a m) c
streamMapAccumLM f a (DVFSM.Stream step t) = DVFSM.Stream (step' step) (a, t)
  where step' :: forall s . (s -> m (DVFSM.Step s b)) -> (a, s) -> StateT a m (DVFSM.Step (a, s) c)
        step' innerStep (a, s) = do
          r <- lift $ innerStep s
          case r of
            DVFSM.Yield b s' -> do
              (a', c') <- lift $ f a b
              liftM  (`DVFSM.Yield` (a', s')) (return c')
            DVFSM.Skip    s' -> return (DVFSM.Skip    (a, s'))
            DVFSM.Done       -> put a >> return DVFSM.Done
        {-# INLINE_INNER step' #-}
{-# INLINE_FUSED streamMapAccumLM #-}
