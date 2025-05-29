{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.NDArray.Types where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Int (Int16, Int32, Int64)
import Telescope.Data.Axes
import Telescope.Data.Binary


{- | In-tree representation of an NDArray. You can parse a file as this and get it back. Not really what we want though
but in haskell we can't easily just parse a multi-dimensional array
we could do a simpler representation. Using an ADT
-}
data NDArrayData = NDArrayData
  { bytes :: ByteString
  , byteorder :: ByteOrder
  , datatype :: DataType
  , shape :: Axes Row
  }
  deriving (Eq)


instance Show NDArrayData where
  show nd = unwords ["NDArrayData", show (BS.length nd.bytes), show nd.byteorder, show nd.shape]


data DataType
  = Float64
  | Float32
  | Int64
  | Int32
  | Int16
  | Int8
  | Bool8
  | Ucs4 Int
  deriving (Show, Eq)


class IsDataType a where
  dataType :: DataType


instance IsDataType Double where
  dataType = Float64
instance IsDataType Float where
  dataType = Float32
instance IsDataType Int64 where
  dataType = Int64
instance IsDataType Int32 where
  dataType = Int32
instance IsDataType Int16 where
  dataType = Int16
instance IsDataType Int8 where
  dataType = Int8
instance (IsDataType a) => IsDataType [a] where
  dataType = dataType @a
