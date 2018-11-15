{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Vector.Storable
  ( padded
  , foldMap
  , mapAccumL
  , mapAccumLViaLazyState
  , mapAccumLViaStrictState
  , mapAccumLFusable
  , mapAccumLViaTranscribe
  ) where

import Control.Monad
import Control.Monad.ST                  (ST)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid                       (Monoid (..), (<>))
import Data.Traversable                  hiding (mapAccumL)
import Data.Tuple
import Data.Vector.Fusion.Bundle         (Bundle, MBundle, inplace)
import Data.Vector.Fusion.Bundle.Size    (Size (Exact))
import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (Stream))
import Data.Vector.Storable              (Storable)
import Data.Word
import Prelude                           hiding (foldMap)

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

transcribe :: Monad m => (s -> a -> (b, s)) -> s -> Stream m a -> Stream m b
transcribe f w (Stream step state) = Stream step' (state, w)
  where step' (t, x) = do
          stepResult <- step t
          return $ case stepResult of
            Yield a s' -> let (z, y) = f x a in Yield z (s', y)
            Skip    s' -> Skip (s', x)
            Done       -> Done
        {-# INLINE step' #-}
{-# INLINE transcribe #-}



mapAccumLViaTranscribe ::  forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumLViaTranscribe f a vs = (undefined, DVG.unstream $ h $ DVG.stream vs)
  where g a b = swap (f a b)
        h :: Bundle DVS.Vector b -> Bundle DVS.Vector c
        h ba = DVFBM.fromStream (transcribe g a (DVFBM.elements ba)) (Exact (DVS.length vs))
{-# INLINE mapAccumLViaTranscribe #-}

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
unstreamM = \s -> do
  xs <- DVFBM.toList s
  return $ DVG.unstream $ DVFB.unsafeFromList (DVFBM.size s) xs

mapAccumLFusable :: forall a b c. (Storable b, Storable c)
  => (a -> b -> (a, c))
  -> a
  -> DVS.Vector b
  -> (a, DVS.Vector c)
mapAccumLFusable = \f a v -> swap $ DVFU.unId $ flip runStateT a $ unstreamM (bundleMapAccumL f (DVG.stream v))
{-# INLINE mapAccumLFusable #-}

-- | Map a function over a 'Bundle'
bundleMapAccumL :: Monad m => (a -> b -> (a, c)) -> DVFBM.Bundle m v b -> DVFBM.Bundle (StateT a m) v c
bundleMapAccumL = \f -> bundleMapAccumLM (\a b -> return (f a b))
{-# INLINE bundleMapAccumL #-}

-- | Map a monadic function over a 'Bundle'
bundleMapAccumLM :: Monad m => (a -> b -> m (a, c)) -> DVFBM.Bundle m v b -> DVFBM.Bundle (StateT a m) v c
bundleMapAccumLM = \f DVFBM.Bundle{DVFBM.sElems = s, DVFBM.sSize = n} -> DVFBM.fromStream (streamMapAccumLM f s) n
{-# INLINE [1] bundleMapAccumLM #-}

streamMapAccumL :: Monad m => (a -> b -> (a, c)) -> DVFSM.Stream m b -> DVFSM.Stream (StateT a m) c
streamMapAccumL = \f -> streamMapAccumLM (\a b -> return (f a b))
{-# INLINE streamMapAccumL #-}

streamMapAccumLM :: forall m a b c . Monad m => (a -> b -> m (a, c)) -> DVFSM.Stream m b -> DVFSM.Stream (StateT a m) c
streamMapAccumLM =  \f (DVFSM.Stream step t) ->
                      let step' :: forall s . (s -> m (DVFSM.Step s b)) -> s -> StateT a m (DVFSM.Step s c)
                          step' = \innerStep s -> StateT $ \a -> do
                            r <- innerStep s
                            case r of
                              DVFSM.Yield b s' -> do
                                (a', c') <- f a b
                                return (DVFSM.Yield c' s', a')
                              DVFSM.Skip    s' -> return (DVFSM.Skip s', a)
                              DVFSM.Done       -> return (DVFSM.Done, a)
                          {-# INLINE [0] step' #-}
                      in DVFSM.Stream (step' step) t
{-# INLINE [1] streamMapAccumLM #-}
