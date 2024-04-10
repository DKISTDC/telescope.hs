module Telescope.Fits
  ( decode
  , encode
  , decodeArray
  , encodeArray

    -- * Headers
  , lookup
  , Header
  , Value
  , LogicalConstant

    -- * Types
  , Fits (..)
  , PrimaryHDU (..)
  , ImageHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes
  , Row
  , Column
  , BitPix

    -- * Exports from Data.Massiv.Array
  , Array
  , Ix1
  , Ix2
  , Ix3
  , Ix4
  , Ix5
  , (!>)
  , (!?>)
  , (<!)
  , (<!?)
  , (<!>)
  , Dim (..)
  ) where

import Data.Fits (lookup)
import Telescope.Fits.Encoding
import Telescope.Fits.Encoding.DataArray
import Telescope.Fits.Types
import Prelude hiding (lookup)

