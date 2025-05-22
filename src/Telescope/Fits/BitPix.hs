{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Fits.BitPix where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Int
import Telescope.Data.Axes
import Telescope.Fits.Header (Header)
import Prelude hiding (lookup)


data BitPix
  = BPInt8
  | BPInt16
  | BPInt32
  | BPInt64
  | BPFloat
  | BPDouble
  deriving (Show, Eq)


bitPixBits :: BitPix -> Int
bitPixBits bp = bitPixBytes bp * 8


bitPixBytes :: BitPix -> Int
bitPixBytes bp =
  case bp of
    BPInt8 -> 1
    BPInt16 -> 2
    BPInt32 -> 4
    BPInt64 -> 8
    BPFloat -> 4
    BPDouble -> 8


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
