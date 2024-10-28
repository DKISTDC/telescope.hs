{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.KnownText where

import Data.Text
import GHC.TypeLits
import Data.Proxy (Proxy (..))


class KnownText a where
  knownText :: Text
instance (KnownSymbol s) => KnownText s where
  knownText = pack $ symbolVal @s Proxy
