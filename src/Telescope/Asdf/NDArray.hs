module Telescope.Asdf.NDArray where

import Telescope.Asdf.Node
import Telescope.Asdf.Parser

import Control.Monad (replicateM)
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, D, Prim, Sz (..))
import Data.Massiv.Array qualified as M
import System.ByteOrder (ByteOrder (..))
import Telescope.Asdf.Error (expected)
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary


-- https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/ndarray-1.1.0.html

class FromNDArray a where
  fromNDArray :: NDArrayData -> Parser a


class ToNDArray a where
  toNDArray :: a -> NDArrayData


instance {-# OVERLAPPABLE #-} (BinaryValue a, IsDataType a) => ToNDArray [a] where
  toNDArray = ndArrayPut shape putBytes
   where
    putBytes = mapM_ (put BigEndian)
    shape as = axesRowMajor [length as]


instance {-# OVERLAPPING #-} (BinaryValue a, IsDataType a) => ToNDArray [[a]] where
  toNDArray = ndArrayPut shape putBytes
   where
    putBytes = mapM_ (mapM_ (put BigEndian))
    shape = axesRowMajor . dimensions
    dimensions [] = []
    dimensions (r1 : rs) =
      [length rs + 1, length r1]


instance {-# OVERLAPPABLE #-} (BinaryValue a) => FromNDArray [a] where
  fromNDArray arr = parseGet (getBytes arr.byteorder arr.shape) arr.bytes
   where
    getBytes bo axes = do
      let num = totalItems axes
      replicateM num (get bo)


instance {-# OVERLAPPING #-} (BinaryValue a) => FromNDArray [[a]] where
  fromNDArray arr = parseGet (getBytes arr.shape) arr.bytes
   where
    getBytes (Axes rows) = mapM getRow rows
    getRow n = replicateM n (get arr.byteorder)


instance (BinaryValue a, Prim a, AxesIndex ix) => FromNDArray (Array D ix a) where
  fromNDArray = parseMassiv


instance (BinaryValue a, IsDataType a, Prim a, AxesIndex ix, PutArray ix) => ToNDArray (Array D ix a) where
  toNDArray = ndArrayMassiv


parseGet :: Get a -> ByteString -> Parser a
parseGet gt bytes =
  case runGetOrFail gt (BL.fromStrict bytes) of
    -- TODO: better error message, including source num? Not sure if it's possible if we do it this way
    Left (rest, nused, err) ->
      fail $ "could not decode binary data at (" ++ show nused ++ ") (rest " ++ show (BL.length rest) ++ "): " ++ err
    Right (_, _, a) -> pure a


parseNDArray :: (FromNDArray a) => Value -> Parser a
parseNDArray val = do
  dat <- ndarray val
  fromNDArray dat
 where
  ndarray (NDArray a) = pure a
  ndarray v = fail $ expected "NDArray" v


ndArrayPut :: forall a. (IsDataType a) => (a -> Axes Row) -> (a -> Put) -> a -> NDArrayData
ndArrayPut toShape putA a =
  let bytes = BL.toStrict $ runPut (putA a)
   in NDArrayData{bytes, byteorder = BigEndian, datatype = dataType @a, shape = toShape a}


ndArrayMassiv :: forall a ix. (IsDataType a, BinaryValue a, Prim a, AxesIndex ix, PutArray ix) => Array D ix a -> NDArrayData
ndArrayMassiv arr =
  let bytes = encodeArray arr
      Sz ix = M.size arr
      shape = indexAxes ix
      datatype = dataType @a
   in NDArrayData{bytes, shape, byteorder = BigEndian, datatype}


parseMassiv :: (BinaryValue a, AxesIndex ix) => NDArrayData -> Parser (Array D ix a)
parseMassiv nda =
  decodeArrayOrder nda.byteorder nda.shape nda.bytes
