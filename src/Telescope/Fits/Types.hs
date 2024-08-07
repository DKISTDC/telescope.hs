module Telescope.Fits.Types
  ( Fits (..)
  , PrimaryHDU (..)
  , ImageHDU (..)
  , BinTableHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes (..)
  , Row
  , Column
  , BitPix (..)
  , bitPixBits
  , Header (..)
  , getKeywords
  , HeaderRecord (..)
  , KeywordRecord (..)
  , Value (..)
  , LogicalConstant (..)
  , hduBlockSize
  , emptyDataArray
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fits (Header (..), HeaderRecord (..), KeywordRecord (..), LogicalConstant (..), Value (..), getKeywords, hduBlockSize)
import Data.List qualified as L
import Telescope.Data.Axes


-- we know the first one is an image
data PrimaryHDU = PrimaryHDU
  { header :: Header
  , dataArray :: DataArray
  }


instance Show PrimaryHDU where
  show p = showHDU "PrimaryHDU" p.header p.dataArray


data ImageHDU = ImageHDU
  { header :: Header
  , dataArray :: DataArray
  }


instance Show ImageHDU where
  show p = showHDU "ImageHDU" p.header p.dataArray


data BinTableHDU = BinTableHDU
  { header :: Header
  , pCount :: Int
  , heap :: ByteString
  , dataArray :: DataArray
  }


instance Show BinTableHDU where
  show p = showHDU "BinTableHDU" p.header p.dataArray


data DataArray = DataArray
  { bitpix :: BitPix
  , axes :: Axes Column
  , rawData :: BS.ByteString
  }


instance Show DataArray where
  show d =
    L.intercalate
      "\n"
      [ "  data: " <> show (BS.length d.rawData) <> " bytes"
      , "  dimensions: "
      , "    format: " <> L.drop 2 (show d.bitpix)
      , "    axes: " <> show d.axes.axes
      ]


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
  "  Header: " <> show (length $ getKeywords h)


emptyDataArray :: DataArray
emptyDataArray = DataArray BPInt8 (Axes []) ""


-- data BinaryTable = BinaryTable
--   { pCount :: Int
--   , heap :: ByteString
--   }

data Extension
  = Image ImageHDU
  | BinTable BinTableHDU


instance Show Extension where
  show (Image i) = show i
  show (BinTable b) = show b


data Fits = Fits
  { primaryHDU :: PrimaryHDU
  , extensions :: [Extension]
  }


instance Show Fits where
  show f =
    show f.primaryHDU
      <> "\n"
      <> L.intercalate "\n" (fmap show f.extensions)


--   L.intercalate
--   "\n"
-- \$ fmap show extensions

data BitPix
  = BPInt8
  | BPInt16
  | BPInt32
  | BPInt64
  | BPFloat
  | BPDouble
  deriving (Show, Eq)


bitPixBits :: BitPix -> Int
bitPixBits BPInt8 = 8
bitPixBits BPInt16 = 16
bitPixBits BPInt32 = 32
bitPixBits BPInt64 = 64
bitPixBits BPFloat = 32
bitPixBits BPDouble = 64
