{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module HaskellWorks.Data.Pattern
  ( pattern Nil
  , pattern (:~:)
  , pattern (:-:)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.Vector.Storable          as DVS
import qualified HaskellWorks.Data.Empty       as HW
import qualified HaskellWorks.Data.Uncons      as HW

pattern (:~:) :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
pattern (:~:) bs bss = LBSI.Chunk bs bss

pattern (:-:) :: HW.Uncons c => HW.Elem c -> c -> c
pattern (:-:) a as <- (HW.uncons -> Just (a, as))

pattern Nil :: (HW.Empty c, HW.Uncons c) => c
pattern Nil <- (HW.uncons -> Nothing) where
  Nil = HW.empty

{-# COMPLETE Nil, (:~:) #-}

{-# COMPLETE (:-:) , Nil :: BS.ByteString #-}
{-# COMPLETE (:-:) , Nil :: DVS.Vector    #-}
