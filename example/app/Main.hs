{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Massiv.Array as M hiding (forM_, zip)
import Data.Massiv.Array.IO
import Telescope.Fits


main :: IO ()
main = do
  inp <- BS.readFile "/Users/seanhess/Downloads/jwst.fits"
  fits <- decode inp

  print fits.extensions

  forM_ (zip @Int [0 ..] fits.extensions) $ \(n, hdu) -> do
    case hdu of
      Image img -> do
        arr <- decodeDataArray @Ix2 @Float img.dataArray
        writeImage ("jwst " <> show n <> ".png") (heatmap arr)
      _ -> pure ()


-- approximates pythons viridis
heatmap :: forall n. (Ord n, RealFrac n) => Array D Ix2 n -> Array D Ix2 (Pixel (SRGB 'NonLinear) Word8)
heatmap arr =
  let maxn = maximum arr :: n
   in M.map (color . (/ maxn)) arr
 where
  color x =
    let r = floor (255 * abs (x - 0.3))
        g = floor (255 * x)
        b = floor (255 * (1 - x))
     in PixelSRGB r g b
