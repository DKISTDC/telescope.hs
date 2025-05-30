module Telescope.Fits.HDU where

import Data.ByteString qualified as BS
import Data.List qualified as L
import Telescope.Data.Axes
import Telescope.Fits.BitPix
import Telescope.Fits.DataArray
import Telescope.Fits.Header.Header


data Fits = Fits
  { primaryHDU :: DataHDU
  , extensions :: [Extension]
  }


instance Show Fits where
  show f =
    show f.primaryHDU
      <> "\n"
      <> L.intercalate "\n" (fmap show f.extensions)


data DataHDU = DataHDU
  { header :: Header
  , dataArray :: DataArray
  }


instance Show DataHDU where
  show p = showHDU "DataHDU" p.header p.dataArray


data BinTableHDU = BinTableHDU
  { header :: Header
  , pCount :: Int
  , heap :: BS.ByteString
  , dataArray :: DataArray
  }


instance Show BinTableHDU where
  show p = showHDU "BinTableHDU" p.header p.dataArray


showHDU :: String -> Header -> DataArray -> String
showHDU name h d =
  L.intercalate
    "\n"
    [ name
    , showHeader h
    , show d
    ]


showHeader :: Header -> String
showHeader h =
  "  Header: " <> show (length $ keywords h) <> " records"


emptyDataArray :: DataArray
emptyDataArray = DataArray BPInt8 (Axes []) ""


data Extension
  = Image DataHDU
  | BinTable BinTableHDU


instance Show Extension where
  show (Image i) = "\nImage: " <> show i
  show (BinTable b) = "\nBinTable: " <> show b
