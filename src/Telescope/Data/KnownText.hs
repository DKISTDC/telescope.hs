{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.KnownText where

import Data.Proxy (Proxy (..))
import Data.Text
import GHC.TypeLits


-- | Types that have a textual representation, like 'KnownSymbol', but for any 'Type'
class KnownText a where
  knownText :: Text


instance (KnownSymbol s) => KnownText s where
  knownText = pack $ symbolVal @s Proxy
