module Telescope.Fits.Encoding.DataArray
  ( -- * Encoding Images
    decodeArray
  , encodeArray

    -- * Encoding as ByteStrings
  , decodeArrayData
  , encodeArrayData

    -- * Handling Axes
  , totalPix
  , AxesIndex (..)
  , getAxesVector
  , runGetThrow
  , sizeAxes

    -- * Binary Encoding
  , GetPix (..)
  , PutPix (..)
  , PutArray (..)
  , parseVector
  , fromVector

    -- * Exports from Data.Massiv.Array
  , Array
  , Ix1
  , Ix2
  , Ix3
  , Ix4
  , Ix5
  , size
  , (!>)
  , (!?>)
  , (<!)
  , (<!?)
  , (<!>)
  , Dim (..)
  ) where

import Control.Exception
import Control.Monad.Catch
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Massiv.Array as M hiding (isEmpty, product)
import Data.Proxy
import Data.Word (Word8)
import Telescope.Fits.Types


-- > {-# LANGUAGE TypeApplications #-}
-- > import Data.Massiv.Array
-- > import Data.Fits.Image
-- > import Data.Fits
-- >
-- > decodeExample :: BL.ByteString -> Either String Int
-- > decodeExample bs = do
-- >  hdu <- readPrimaryHDU bs
-- >  arr <- decodeImage @Ix2 $ hdu.dataArray
-- >  pure $ arr !> 1 ! 2

{- | Decode a 'DataArray' of arbitrary dimensions 'ix' and type 'a'. Inspect the DataArray's (.bitpix) and (.axes) if these are unknown.

>>> decodeArray @Ix2 @Float hdu.dataArray
Array D Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]

This creates a delayed (D) array, which will postpone evaluation of cells until needed
-}
decodeArray :: forall ix a m. (MonadThrow m) => (Index ix, AxesIndex ix, Prim a, GetPix a) => DataArray -> m (Array D ix a)
decodeArray DataArray{bitpix, axes, rawData} = do
  decodeArrayData bitpix axes rawData


{- | Decode data into an Array of arbitrary dimensions 'ix' specifying 'BitPixFormat' and 'Axes'

>>> decodeArray @Ix2 @Float BPFloat [3, 2] input
Array P Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]
-}
decodeArrayData
  :: forall ix a m
   . (AxesIndex ix, Prim a, GetPix a, Index ix, MonadThrow m)
  => BitPix
  -> Axes Column
  -> BS.ByteString
  -> m (Array D ix a)
decodeArrayData f as inp = do
  let v = parseVector @a Par f inp
  fromVector as v


fromVector :: forall ix a m. (AxesIndex ix, Index ix, Prim a, MonadThrow m) => Axes Column -> Vector D a -> m (Array D ix a)
fromVector as v = do
  ix <- axesIndex $ rowMajor as
  resizeM (Sz ix) v


-- TODO: switch to throwable
-- TODO: generialize this to work for both Fits and Asdf
parseVector :: forall a. (GetPix a) => Comp -> BitPix -> BS.ByteString -> Vector D a
parseVector c bp inp =
  let v = parseWordVector inp
   in makeArray c (bitPixSize v) (toBitPix v)
 where
  bitPixSize v =
    let Sz s = size v
     in Sz $ s `div` bytes

  bytes = bitPixBits bp `div` 8

  toBitPix :: Vector D Word8 -> Ix1 -> a
  toBitPix v ix =
    let slc = M.slice (ix * bytes) (Sz bytes) v :: Vector D Word8
     in -- in trace (show ("toBitPix:" <> show ix, show bytes, show slc)) $
        toPix $ M.toList slc

  parseWordVector :: BS.ByteString -> Vector D Word8
  parseWordVector = fromByteString c

  toPix :: [Word8] -> a
  toPix ws =
    -- trace (show (ws, BL.pack ws)) $
    case runGetOrFail (getPix @a bp) (BL.pack ws) of
      Left (ip, byts, e) -> throw $ ParseError byts (e <> " " <> show ip <> show bp)
      Right (_, _, a) -> a


-- | Decode Axes as a delayed 1d vector
getAxesVector :: Get a -> Axes Column -> Get (Vector DS a)
getAxesVector get as = do
  sreplicateM (Sz1 (totalPix as)) get


runGetThrow :: forall a m. (MonadThrow m) => Get a -> BL.ByteString -> m a
runGetThrow get inp =
  case runGetOrFail get inp of
    Left (_, bytes, e) -> throwM $ ParseError bytes e
    Right (_, _, a) -> pure a


----- ENCODING -----------------------------------------------------------------

{- | Encode an 'Array' to a 'DataArray'

>>> encodeImage array
DataArray:
  data: 48 bytes
  dimensions:
    format: Int64
    axes: [3,2]
-}
encodeArray
  :: forall r ix a
   . (Source r a, Stream r Ix1 a, Size r, PutArray ix, Index ix, AxesIndex ix, PutPix a, Prim a)
  => Array r ix a
  -> DataArray
encodeArray arr =
  let axes = sizeAxes $ size arr
      bitpix = bitPixFormat @a Proxy
      rawData = encodeArrayData arr -- O(n)
   in DataArray{bitpix, axes, rawData}


{- | Encode an Array as a Lazy ByteString based on the type of the element 'a'

>>> myArray = decodeArray @Ix2 @Float BPFloat [3, 2] input
>>> output = encodeArray myArray
-}
encodeArrayData :: (Source r a, Stream r Ix1 a, PutArray ix, Index ix, PutPix a, Prim a) => Array r ix a -> BS.ByteString
encodeArrayData = BL.toStrict . runPut . putArray


-- | The total number of pixels to read from the input ByteString
totalPix :: Axes Column -> Int
totalPix (Axes as) = product as


class AxesIndex ix where
  axesIndex :: (MonadThrow m) => Axes Row -> m ix
  indexAxes :: ix -> Axes Row


instance AxesIndex Ix1 where
  axesIndex (Axes [i]) = pure i
  axesIndex as = throwM $ AxesMismatch as
  indexAxes n = Axes [n]


instance AxesIndex Ix2 where
  axesIndex (Axes [c, r]) = do
    ix1 <- axesIndex $ Axes [r]
    pure $ c :. ix1
  axesIndex as = throwM $ AxesMismatch as
  indexAxes (c :. r) = Axes [c, r]


instance AxesIndex Ix3 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN


instance AxesIndex Ix4 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN


instance AxesIndex Ix5 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN


axesIndexN :: (AxesIndex (Lower (IxN n))) => (MonadThrow m) => Axes Row -> m (IxN n)
axesIndexN (Axes (a : as)) = do
  ixl <- axesIndex (Axes as)
  pure $ a :> ixl
axesIndexN as = throwM $ AxesMismatch as


indexAxesN :: (AxesIndex (Lower (IxN n))) => IxN n -> Axes Row
indexAxesN (d :> ix) =
  let Axes ax = indexAxes ix
   in Axes $ d : ax


sizeAxes :: (AxesIndex ix, Index ix) => Sz ix -> Axes Column
sizeAxes (Sz ix) = columnMajor $ indexAxes ix


data ParseError
  = ParseError !ByteOffset !String
  | AxesMismatch !(Axes Row)
  deriving (Show, Exception)


class GetPix a where
  getPix :: BitPix -> Get a


instance GetPix Int8 where
  getPix BPInt8 = getInt8
  getPix f = fail $ "Expected Int8, but format is " <> show f


instance GetPix Int16 where
  getPix BPInt16 = getInt16be
  getPix f = fail $ "Expected Int16, but format is " <> show f


instance GetPix Int32 where
  getPix BPInt32 = getInt32be
  getPix f = fail $ "Expected Int32, but format is " <> show f


instance GetPix Int64 where
  getPix BPInt64 = getInt64be
  getPix f = fail $ "Expected Int64, but format is " <> show f


instance GetPix Int where
  getPix BPInt8 = fromIntegral <$> getPix @Int8 BPInt8
  getPix BPInt16 = fromIntegral <$> getPix @Int16 BPInt16
  getPix BPInt32 = fromIntegral <$> getPix @Int32 BPInt32
  getPix BPInt64 = fromIntegral <$> getPix @Int64 BPInt64
  getPix f = fail $ "Expected Int, but format is " <> show f


instance GetPix Float where
  getPix BPFloat = getFloatbe
  getPix f = fail $ "Expected Float, but format is " <> show f


instance GetPix Double where
  getPix BPDouble = getDoublebe
  getPix f = fail $ "Expected Double, but format is " <> show f


-- | How to encode an element type. Note that there is no instance for 'Int', since the size is system dependent. Use Int64 or Int32 instead
class PutPix a where
  bitPixFormat :: Proxy a -> BitPix
  putPix :: a -> Put


instance PutPix Int8 where
  bitPixFormat _ = BPInt8
  putPix = putInt8
instance PutPix Int16 where
  bitPixFormat _ = BPInt16
  putPix = putInt16be
instance PutPix Int32 where
  bitPixFormat _ = BPInt32
  putPix = putInt32be
instance PutPix Int64 where
  bitPixFormat _ = BPInt64
  putPix = putInt64be
instance PutPix Float where
  bitPixFormat _ = BPFloat
  putPix = putFloatbe
instance PutPix Double where
  bitPixFormat _ = BPDouble
  putPix = putDoublebe


class PutArray ix where
  putArray :: (Source r a, Stream r Ix1 a, PutPix a, Prim a) => Array r ix a -> Put


instance PutArray Ix1 where
  putArray = sfoldl (\b a -> b <> putPix a) mempty


instance PutArray Ix2 where
  putArray = foldOuterSlice putArray


instance PutArray Ix3 where
  putArray = foldOuterSlice putArray


instance PutArray Ix4 where
  putArray = foldOuterSlice putArray


instance PutArray Ix5 where
  putArray = foldOuterSlice putArray
