{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE ViewPatterns    #-}

module HaskellWorks.Data.Ops
  ( (<|)
  , (|>)
  , (><)
  , pattern (:<|)
  , pattern (:|>)
  , pattern Empty
  ) where

import Data.Semigroup (Semigroup, (<>))

import qualified HaskellWorks.Data.Cons      as HW
import qualified HaskellWorks.Data.Container as HW
import qualified HaskellWorks.Data.Empty     as HW
import qualified HaskellWorks.Data.Null      as HW
import qualified HaskellWorks.Data.Snoc      as HW
import qualified HaskellWorks.Data.Uncons    as HW
import qualified HaskellWorks.Data.Unsnoc    as HW

infixr 5 ><

infixr 5 :<|
infixr 5 <|

infixl 5 :|>
infixl 5 |>

(<|) :: HW.Cons v => HW.Elem v -> v -> v
(<|) = HW.cons
{-# INLINE (<|) #-}

(|>) :: HW.Snoc v => v -> HW.Elem v -> v
(|>) = HW.snoc
{-# INLINE (|>) #-}

(><) :: (Semigroup v, HW.Container v) => v -> v -> v
(><) = (<>)
{-# INLINE (><) #-}

pattern (:<|) :: (HW.Cons v, HW.Uncons v) => HW.Elem v -> v -> v
pattern a :<| b <- (HW.uncons -> Just (a, b)) where
  a :<| b = HW.cons a b

pattern (:|>) :: (HW.Snoc v, HW.Unsnoc v) => HW.Elem v -> v -> v
pattern a :|> b <- (HW.unsnoc -> Just (a, b))
  where a :|> b = HW.snoc b a

pattern Empty :: (HW.Null v, HW.Empty v) => v
pattern Empty <- (HW.null -> True)
  where Empty = HW.empty

-- TODO How?
-- {-# COMPLETE (:<|), Empty #-}
-- {-# COMPLETE (:|>), Empty #-}
