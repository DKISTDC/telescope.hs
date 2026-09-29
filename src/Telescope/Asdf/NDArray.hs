{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

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
  , getAscii
  , Parser
  )
where

import Control.Monad (forM, replicateM)
import Control.Monad.Catch (try)
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Massiv.Array (Array, D, Prim, Sz (..))
import Data.Massiv.Array qualified as M
import Data.Scientific (fromFloatDigits)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace (traceM)
import Effectful
import GHC.Int
import Telescope.Asdf.NDArray.Types
import Telescope.Asdf.Node
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Data.Parser


-- import Telescope.Asdf.Node

{- | Convert an 'NDArrayData' into a type
https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/ndarray-1.1.0.html
-}
class FromNDArray a where
  fromNDArray :: (Parser :> es) => NDArrayData -> Eff es a


{- | Convert a type to an 'NDArrayData'
https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/ndarray-1.1.0.html
-}
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


instance ToNDArray [Text] where
  toNDArray ts =
    let len = maxLength ts
        bytes = BL.toStrict $ runPut $ mapM_ (putUcs4 len) ts
        shape = Axes [length ts]
     in NDArrayData{bytes, byteorder = BigEndian, datatype = Ucs4 len, shape}
   where
    maxLength [] = 0
    maxLength _ = maximum $ fmap T.length ts


instance FromNDArray [Text] where
  fromNDArray arr = do
    case arr.datatype of
      Ucs4 n -> fromUcs4 n
      Ascii n -> fromAscii n
      t -> parseFail $ "FromNDArray from non-text datatype: " <> show t
   where
    fromUcs4 n = do
      parseGet (replicateM (totalItems arr.shape) (getUcs4 arr.byteorder n)) arr.bytes

    fromAscii n = do
      values <- parseGet (replicateM (totalItems arr.shape) (getAscii arr.byteorder n)) arr.bytes
      forM values $ \res -> do
        case res of
          (ascii, "") -> pure ascii
          (ascii, bytes) -> parseFail $ "Ascii has (" <> show (BS.length bytes) <> ") remaining bytes after prefix: " <> show ascii


-- case T.decodeASCIIPrefix arr.bytes of
--   (ascii, "") -> pure ascii
--   (prefix, bytes) ->

instance FromNDArray [Value] where
  fromNDArray arr = do
    case arr.datatype of
      Float64 -> fmap (Number . fromFloatDigits) <$> fromNDArray @[Double] arr
      Float32 -> fmap (Number . fromFloatDigits) <$> fromNDArray @[Float] arr
      Bool8 -> fmap Bool <$> fromNDArray @[Bool] arr
      Ucs4 _ -> fmap String <$> fromNDArray @[Text] arr
      Ascii _ -> fmap String <$> fromNDArray @[Text] arr
      _int -> fmap (Integer . fromIntegral) <$> fromNDArray @[Int64] arr


instance {-# OVERLAPS #-} FromNDArray [String] where
  fromNDArray arr = do
    fmap unpack <$> fromNDArray @[Text] arr


instance FromNDArray [BS.ByteString] where
  fromNDArray arr = do
    fmap BSC.pack <$> fromNDArray @[String] arr


instance FromNDArray [BL.ByteString] where
  fromNDArray arr = do
    fmap BLC.pack <$> fromNDArray @[String] arr


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


getAscii :: ByteOrder -> Int -> Get (Text, ByteString)
getAscii bo n = do
  decodeAscii <$> getByteString n
 where
  decodeAscii bs =
    case bo of
      BigEndian -> T.decodeASCIIPrefix . BS.dropWhileEnd (== 0x0) $ bs
      LittleEndian -> T.decodeASCIIPrefix . BS.dropWhile (== 0x0) $ bs


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
