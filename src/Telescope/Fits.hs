{- |
Module:      Telescope.Fits
Copyright:   (c) 2024 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <shess@nso.edu>
Stability:   experimental
Portability: portable

Read, Generate, and Write FITS (Flexible Image Transport System) files

@
import Data.ByteString qualified as BS
import Telescope.Fits

test :: IO ()
test = do
  inp <- BS.readFile "samples/simple2x3.fits"
  f <- decode inp
  print f.primaryHDU.dataArray.axes
  print f.primaryHDU.dataArray.bitpix
  print $ lookupKeyword \"BTYPE\" f.primaryHDU.header

  a <- decodeArray @Ix2 @Int f.primaryHDU.dataArray
  print $ size a
  print $ a !> 0
@
-}
module Telescope.Fits
  ( decode
  , encode
  , decodeDataArray
  , encodeDataArray

    -- * Headers
  , lookupKeyword
  , Header
  , Value (..)
  , LogicalConstant (..)

    -- * Parsing Headers
  , FromHeader (..)
  , FromKeyword (..)

    -- * Creating Headers
  , ToKeyword (..)
  , ToHeader (..)
  , Parser

    -- * Types
  , Fits (..)
  , KeywordRecord (..)
  , DataHDU (..)
  , BinTableHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes
  , Major (..)
  , BitPix (..)

    -- * Generate
  , addComment
  , dataArray
  , emptyDataArray

    -- * Visualize
  , heatmap
  , writeImage

    -- * Exports from Data.Massiv.Array
  , Array
  , D
  , Ix1
  , Ix2
  , Ix3
  , Ix4
  , Ix5
  , size
  , (!>)
  , (!?>)
  , (<!)
  , (<!?)
  , (<!>)
  , Dim (..)
  -- , test
  ) where

import Data.Massiv.Array (Array, D, Dim (..), Ix1, Ix2, Ix3, Ix4, Ix5, size, (!>), (!?>), (<!), (<!>), (<!?))
import Data.Massiv.Array.IO (writeImage)
import Telescope.Data.Axes
import Telescope.Data.Parser (Parser)
import Telescope.Data.Array (heatmap)
import Telescope.Fits.BitPix
import Telescope.Fits.DataArray
import Telescope.Fits.Encoding
import Telescope.Fits.HDU
import Telescope.Fits.Header

