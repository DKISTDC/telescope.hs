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

test :: IO ()
test = do
  inp <- BS.readFile "samples/simple2x3.fits"
  f <- decode inp
  print f.primaryHDU.dataArray.axes
  print f.primaryHDU.dataArray.bitpix
  print $ lookup \"BTYPE\" f.primaryHDU.header

  a <- decodeArray @Ix2 @Int f.primaryHDU.dataArray
  print $ size a
  print $ a !> 0
@
-}
module Telescope.Fits
  ( decode
  , encode
  , decodeArray
  , encodeArray

    -- * Headers
  , lookup
  , Header (..)
  , Value (..)
  , LogicalConstant

    -- * Types
  , Fits (..)
  , PrimaryHDU (..)
  , ImageHDU (..)
  , BinTableHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes
  , Row
  , Column
  , BitPix (..)

    -- * Generate
  , addComment
  , keyword
  , emptyDataArray

    -- * Exports from Data.Massiv.Array
  , Array
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
  , test
  ) where

import Data.Fits (lookup)
import Telescope.Fits.Encoding
import Telescope.Fits.Encoding.DataArray
import Telescope.Fits.Header (addComment, keyword)
import Telescope.Fits.Types
import Prelude hiding (lookup)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Telescope.Fits.Encoding.Checksum


-- test :: IO ()
-- test = do
--   inp <- BS.readFile "samples/simple2x3.fits"
--   f <- decode inp
--   print f.primaryHDU.dataArray.axes
--   print f.primaryHDU.dataArray.bitpix
--   print f.primaryHDU.dataArray
--   print $ lookup "BTYPE" f.primaryHDU.header
--
--   a <- decodeArray @Ix2 @Int f.primaryHDU.dataArray
--   print $ size a
--   print $ a !> 0
--
--
-- test2 :: IO ()
-- test2 = do
--   inp <- BS.readFile "./samples/dkist4x4.fits"
--   f <- decode inp
--   print f.primaryHDU.header
--   [BinTable b] <- pure f.extensions
--   print b.header

test :: IO ()
test = do
  inp <- BS.readFile "./samples/ones10x10.fits"
  print $ BS.length inp
  fits <- decode inp
  let hdu = fits.primaryHDU
  -- let [Image hdu] = fits.extensions
  -- let out = BL.toStrict $ runRender $ renderData hdu.dataArray.rawData
  print $ BS.length hdu.dataArray.rawData
  print $ datasum hdu.dataArray.rawData
