{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.NDArray
  ( NDArrayData (..)
  , FromNDArray (..)
  , ToNDArray (..)
  , DataType (..)
  , IsDataType (..)
  , parseGet
  , ndArrayPut
  , ndArrayMassiv
  , parseMassiv
  , parseNDArray
  , ByteOrder (..)
  , getUcs4
  , putUcs4
  , Parser
  )
where

import Control.Monad (replicateM)
import Control.Monad.Catch (try)
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, D, Prim, Sz (..))
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Effectful
import Telescope.Asdf.NDArray.Types
import Telescope.Asdf.Node
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Data.Parser


-- import Telescope.Asdf.Node

-- https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/ndarray-1.1.0.html

class FromNDArray a where
  fromNDArray :: (Parser :> es) => NDArrayData -> Eff es a


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


instance FromNDArray [Text] where
  fromNDArray arr = do
    n <- fromIntegral <$> ucs4Size arr.datatype
    parseGet (replicateM (totalItems arr.shape) (getUcs4 arr.byteorder n)) arr.bytes
   where
    ucs4Size = \case
      Ucs4 n -> pure n
      dt -> expected "Ucs4" dt


-- decode LittleEndian = T.decodeUtf32LE
-- decode BigEndian = T.decodeUtf32BE

instance {-# OVERLAPPING #-} (BinaryValue a) => FromNDArray [[a]] where
  fromNDArray arr = parseGet (getBytes arr.shape) arr.bytes
   where
    getBytes (Axes rows) = mapM getRow rows
    getRow n = replicateM n (get arr.byteorder)


instance (BinaryValue a, Prim a, AxesIndex ix) => FromNDArray (Array D ix a) where
  fromNDArray = parseMassiv


instance (BinaryValue a, IsDataType a, Prim a, AxesIndex ix, PutArray ix) => ToNDArray (Array D ix a) where
  toNDArray = ndArrayMassiv


parseGet :: (Parser :> es) => Get a -> ByteString -> Eff es a
parseGet gt bytes =
  case runGetOrFail gt (BL.fromStrict bytes) of
    Left (rest, nused, err) ->
      parseFail $ "could not decode binary data at (" ++ show nused ++ ") (rest " ++ show (BL.length rest) ++ "): " ++ err
    Right (_, _, a) -> pure a


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


parseMassiv :: (BinaryValue a, AxesIndex ix, Parser :> es) => NDArrayData -> Eff es (Array D ix a)
parseMassiv nda = do
  ea <- try $ decodeArrayOrder nda.byteorder nda.shape nda.bytes
  case ea of
    Left (e :: ArrayError) -> parseFail $ show e
    Right a -> pure a


putUcs4 :: Int -> Text -> Put
putUcs4 n t = putByteString $ justifyUcs4 n $ T.encodeUtf32BE t


getUcs4 :: ByteOrder -> Int -> Get Text
getUcs4 bo n =
  decodeUcs4 <$> getByteString (n * 4)
 where
  decodeUcs4 bs =
    case bo of
      BigEndian -> T.decodeUtf32BE . BS.dropWhileEnd (== 0x0) $ bs
      LittleEndian -> T.decodeUtf32LE . BS.dropWhile (== 0x0) $ bs


justifyUcs4 :: Int -> BS.ByteString -> BS.ByteString
justifyUcs4 len bs =
  let nulls = len * 4 - BS.length bs
   in bs <> BS.replicate nulls 0x0


parseNDArray :: (FromNDArray a, Parser :> es) => Value -> Eff es a
parseNDArray val = do
  dat <- ndarray val
  fromNDArray dat
 where
  ndarray (NDArray a) = pure a
  ndarray v = expected "NDArray" v
