{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.ByteString qualified as BS
import Data.Int
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Telescope.Asdf as Asdf
import Telescope.Data.Parser
import Telescope.Fits as Fits


main :: IO ()
main = do
  example
  mainASDF


-- mainFITS

data DKISTInfo = DKISTInfo
  { instrumentName :: Text
  , frameCount :: Int
  }
  deriving (Show, Generic, FromAsdf)


data Dataset = Dataset
  { info :: DKISTInfo
  }
instance FromAsdf Dataset where
  -- parse .dataset.meta.inventory into DKISTInfo
  parseValue val =
    case val of
      Object o -> do
        dset <- o .: "dataset"
        meta <- dset .: "meta"
        info <- meta .: "inventory"
        pure $ Dataset info
      k -> expected "Object" k


mainASDF :: IO ()
mainASDF = do
  inp <- BS.readFile "../samples/dkist.asdf"
  dset :: Dataset <- decodeM inp
  print dset.info


data Example = Example
  { name :: Text
  , items :: [Text]
  , sequence :: [Int64]
  , random :: Array D Ix1 Double
  }
  deriving (Generic, FromAsdf)


example :: IO ()
example = do
  inp <- BS.readFile "../samples/example.asdf"
  ex :: Example <- decodeM inp
  print ex.name
  print ex.items
  print $ take 30 ex.sequence
  print $ take 10 $ M.toList ex.random


mainFITS :: IO ()
mainFITS = do
  inp <- BS.readFile "/Users/seanhess/Downloads/hubble.fits"
  fits <- Fits.decode inp

  print fits.primaryHDU

  let h = fits.primaryHDU.header
  -- print h

  print $ lookupKeyword "TELESCOP" h

  mapM_ print fits.extensions

  Image img : _ <- pure fits.extensions
  arr <- decodeDataArray @Ix2 @Float img.dataArray
  writeImage "hubble1.png" (heatmap arr)
