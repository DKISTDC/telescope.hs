{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Fits.Types
  ( Fits (..)
  , PrimaryHDU (..)
  , ImageHDU (..)
  , BinTableHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes (..)
  , Major (..)
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
  , IsBitPix (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fits (Header (..), HeaderRecord (..), KeywordRecord (..), LogicalConstant (..), Value (..), getKeywords, hduBlockSize)
import Data.List qualified as L
import GHC.Int
import Telescope.Data.Axes


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


-- | Raw HDU Data. See 'Telescope.Fits.DataArray'
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


class IsBitPix a where
  bitPix :: BitPix


instance IsBitPix Int8 where
  bitPix = BPInt8
instance IsBitPix Int16 where
  bitPix = BPInt16
instance IsBitPix Int32 where
  bitPix = BPInt32
instance IsBitPix Int64 where
  bitPix = BPInt64
instance IsBitPix Float where
  bitPix = BPFloat
instance IsBitPix Double where
  bitPix = BPDouble
