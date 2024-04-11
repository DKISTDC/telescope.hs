module Telescope.Fits.Types
  ( Fits (..)
  , PrimaryHDU (..)
  , ImageHDU (..)
  , DataArray (..)
  , Extension (..)
  , Axis
  , Axes (..)
  , Row
  , Column
  , rowMajor
  , columnMajor
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

import Data.ByteString as BS
import Data.Fits (Header (..), HeaderRecord (..), KeywordRecord (..), LogicalConstant (..), Value (..), getKeywords, hduBlockSize)
import Data.List qualified as L


-- we know the first one is an image
data PrimaryHDU = PrimaryHDU
  { header :: Header
  , dataArray :: DataArray
  }


data ImageHDU = ImageHDU
  { header :: Header
  , dataArray :: DataArray
  }


data DataArray = DataArray
  { bitpix :: BitPix
  , axes :: Axes Column
  , rawData :: BS.ByteString
  }


emptyDataArray :: DataArray
emptyDataArray = DataArray BPInt8 (Axes []) ""


-- data BinaryTable = BinaryTable
--   { pCount :: Int
--   , heap :: ByteString
--   }

data Extension
  = Image ImageHDU


type Axis = Int
newtype Axes a = Axes {axes :: [Axis]}
  deriving (Show, Eq)
data Row
data Column


rowMajor :: Axes Column -> Axes Row
rowMajor (Axes as) = Axes (L.reverse as)


columnMajor :: Axes Row -> Axes Column
columnMajor (Axes as) = Axes (L.reverse as)


data Fits = Fits
  { primaryHDU :: PrimaryHDU
  , extensions :: [Extension]
  }


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
