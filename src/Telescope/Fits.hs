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
  ) where

import Data.Fits (lookup)
import Telescope.Fits.Encoding
import Telescope.Fits.Encoding.DataArray
import Telescope.Fits.Header (addComment, keyword)
import Telescope.Fits.Types
import Prelude hiding (lookup)


-- import Telescope.Fits.Checksum

-- import Data.ByteString qualified as BS
-- import Data.Fits (HeaderDataUnit (..))
-- import Data.Fits.MegaParser (dataSize, parseHDU)
-- import Data.Fits.Read (readHDUs, readPrimaryHDU)
-- import Text.Megaparsec qualified as M
-- import Effectful
-- import Effectful.State.Static.Local

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

-- test :: IO ()
-- test = do
--   putStrLn "\nREADING"
--   inp <- BS.readFile "/Users/seanhess/data/scan1807/inv_res_pre.fits"
--   fits <- decode inp
--   print fits.primaryHDU.dataArray
--   [Image h2, Image h3] <- pure fits.extensions
--   print h2.dataArray
--   print h3.dataArray
--
--   pure ()

-- print $ checksumValue $ checksum out

-- The checksum for the HDU should now be zero

-- putStrLn "\nCHECKSUMMING"
-- print $ encodeChecksum (Checksum 1234)

-- inp <- BS.readFile "/Users/seanhess/data/scan1807/inv_res_pre.fits"
-- print $ BS.length inp

-- -- print $ length $ BS.unpack inp
--
-- putStrLn "\nCHECKSUM"
--
-- print $ checksum $ BS.replicate 2880 0
-- print $ checksum $ BS.replicate 2880 maxBound
-- print $ checksum $ BS.replicate 100 0
--
-- print $ checksum inp

-- print $ length $ words32 inp
-- let n = 299476800
-- let n = 10000000
-- --
-- -- PERF: takes 60s
-- -- let w32 = words32 inp :: [Word32]
--
-- -- PERF: 2s
-- let ns = replicate n (0xFFFFFFFF :: Word32)
--
-- -- print $ length ns
-- -- print $ sum32 ns
-- -- print $ sum32 $ replicate 299476800 (maxBound :: Word32)
-- print $ foldl' woot 0 $ replicate n (maxBound :: Word32)
--
-- where
--
--   woot a b = add1s' a b
-- woot a b = a + b
--
-- print $ sum32 $ words32 inp
-- fits <- decode inp
-- let hdu = fits.primaryHDU
-- -- let [Image hdu] = fits.extensions
-- -- let out = BL.toStrict $ runRender $ renderData hdu.dataArray.rawData
-- print $ BS.length hdu.dataArray.rawData
-- print $ datasum hdu.dataArray.rawData
