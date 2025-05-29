{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.ByteString qualified as BS
import Telescope.Fits

main :: IO ()
main = do
  fits

fits :: IO ()
fits = do
  inp <- BS.readFile "/Users/seanhess/Downloads/hubble.fits"
  fits <- decode inp

  print fits.primaryHDU

  let h = fits.primaryHDU.header
  -- print h

  print $ lookupKeyword "TELESCOP" h

  mapM_ print fits.extensions

  Image img : _ <- pure fits.extensions
  arr <- decodeDataArray @Ix2 @Float img.dataArray
  writeImage "hubble1.png" (heatmap arr)
