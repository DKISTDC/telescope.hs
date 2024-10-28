{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.WCS where

import Data.Text
import GHC.Generics (Generic)
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.KnownText


newtype CUnit = CUnit Text deriving (Eq, Show)
newtype CType = CType Text deriving (Eq, Show)


data WCSAxis (alt :: WCSAlt) axis = WCSAxis
  { ctype :: CType
  , cunit :: CUnit
  , crpix :: Float
  , crval :: Float
  , cdelt :: Float
  }
  deriving (Generic, Eq, Show)


data WCSAlt
  = WCSMain
  | A
instance KnownText WCSMain where
  knownText = ""
instance KnownText A where
  knownText = "A"


-- | Converts a wcs keyword like "ctype" to "CTYPE1A" for header parsing
toWCSAxisKey :: forall alt ax. (KnownText alt, AxisOrder ax) => Text -> Text
toWCSAxisKey = addAlt . addAxisN
 where
  addAxisN k = k <> pack (show (axisN @ax))
  addAlt k = k <> (knownText @alt)
