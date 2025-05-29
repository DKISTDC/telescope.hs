module Telescope.Data.Array where

import Control.Exception (throw)
import Control.Monad.Catch
import Data.Binary.Get (ByteOffset, runGetOrFail)
import Data.Binary.Put (Put, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, Comp (..), D, Index, Ix1, Ix2 (..), Ix3, Ix4, Ix5, IxN (..), Lower, Prim, Source, Stream, Sz (..), Vector)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO (Linearity (..), Pixel (..), SRGB)
import Data.Massiv.Array.IO qualified as M
import Data.Word (Word8)
import Telescope.Data.Axes
import Telescope.Data.Binary


{- | Decode Big Endian encoded binary data into an Array of arbitrary dimensions 'ix' by specifying the 'Axes' 'Row'

>>> decodeArray @Ix2 @Float [3, 2] input
Array P Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]
-}
decodeArray
  :: forall ix a m
   . (AxesIndex ix, Prim a, BinaryValue a, MonadThrow m, MonadCatch m)
  => Axes Row
  -> BS.ByteString
  -> m (Array D ix a)
decodeArray = decodeArrayOrder BigEndian


{- | Encode an Array as a Lazy ByteString based on the type of the element 'a'

>>> myArray = decodeArray @Ix2 @Float BPFloat [3, 2] input
>>> output = encodeArray myArray
-}
encodeArray
  :: (Source r a, Stream r Ix1 a, PutArray ix, BinaryValue a, Prim a)
  => Array r ix a
  -> BS.ByteString
encodeArray = BL.toStrict . runPut . putArray BigEndian


{- | Decode binary data into an Array of arbitrary dimensions 'ix' by specifying the 'Axes' 'Row' and the 'ByteOrder'

>>> decodeArrayOrder @Ix2 @Float BigEndian [3, 2] input
Array P Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]
-}
decodeArrayOrder
  :: forall ix a m
   . (AxesIndex ix, BinaryValue a, MonadThrow m, MonadCatch m)
  => ByteOrder
  -> Axes Row
  -> BS.ByteString
  -> m (Array D ix a)
decodeArrayOrder bo as inp = do
  fromVector as $ decodeVector @a Par bo inp


-- | Decode binary data into as a 1d Array
decodeVector
  :: forall a
   . (BinaryValue a)
  => Comp
  -> ByteOrder
  -> BS.ByteString
  -> Vector D a
decodeVector c bo inp =
  let v = parseWordVector inp
   in M.makeArray c (arraySize v) (valueAt v)
 where
  numBytes = byteSize @a

  arraySize v =
    let Sz s = M.size v
     in Sz $ s `div` numBytes

  valueAt :: Vector D Word8 -> Ix1 -> a
  valueAt v ix =
    fromWords $ wordsAt v ix

  wordsAt :: Vector D Word8 -> Ix1 -> [Word8]
  wordsAt v ix =
    M.toList $ M.slice (ix * numBytes) (Sz numBytes) v

  parseWordVector :: BS.ByteString -> Vector D Word8
  parseWordVector = M.fromByteString c

  -- this should never actually fail
  fromWords :: [Word8] -> a
  fromWords ws =
    case runGetOrFail (get bo) (BL.pack ws) of
      Left (ip, byts, e) -> throw $ BinaryParseError byts (e <> " " <> show ip <> show ws)
      Right (_, _, a) -> a


-- | Resize a Vector into an Array
fromVector
  :: forall ix a m
   . (AxesIndex ix, MonadThrow m, MonadCatch m)
  => Axes Row
  -> Vector D a
  -> m (Array D ix a)
fromVector as v = do
  ix <- axesIndex as
  ea <- try $ M.resizeM (Sz ix) v
  case ea of
    Left (e :: M.SizeException) -> throwM $ ResizeMismatch (show e)
    Right a -> pure a


data ArrayError
  = BinaryParseError !ByteOffset String
  | AxesMismatch !(Axes Row)
  | ResizeMismatch String
  deriving (Show, Exception)


-- | Convert between an 'Index' and 'Axes' 'Row'
class (Index ix) => AxesIndex ix where
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


-- | Encode an 'Array' of dimensions up to 'Ix5'
class PutArray ix where
  putArray
    :: (BinaryValue a, Source r a, Stream r Ix1 a, Prim a)
    => ByteOrder
    -> Array r ix a
    -> Put


instance PutArray Ix1 where
  putArray bo = M.sfoldl (\b a -> b <> put bo a) mempty
instance PutArray Ix2 where
  putArray = M.foldOuterSlice . putArray
instance PutArray Ix3 where
  putArray = M.foldOuterSlice . putArray
instance PutArray Ix4 where
  putArray = M.foldOuterSlice . putArray
instance PutArray Ix5 where
  putArray = M.foldOuterSlice . putArray


axesIndexN :: (AxesIndex (Lower (IxN n))) => (MonadThrow m) => Axes Row -> m (IxN n)
axesIndexN (Axes (a : as)) = do
  ixl <- axesIndex (Axes as)
  pure $ a :> ixl
axesIndexN as = throwM $ AxesMismatch as


indexAxesN :: (AxesIndex (Lower (IxN n))) => IxN n -> Axes Row
indexAxesN (d :> ix) =
  let Axes ax = indexAxes ix
   in Axes $ d : ax


-- | Get the Axes for a given array size
sizeAxes :: (AxesIndex ix, Index ix) => Sz ix -> Axes Column
sizeAxes (Sz ix) = toColumnMajor $ indexAxes ix


-- approximates python's viridis
heatmap :: forall n. (Ord n, RealFrac n) => Array D Ix2 n -> Array D Ix2 (Pixel (SRGB 'NonLinear) Word8)
heatmap arr =
  let maxn = maximum arr :: n
   in fmap (color . (/ maxn)) arr
 where
  color x =
    let r = floor (255 * abs (x - 0.3))
        g = floor (255 * x)
        b = floor (255 * (1 - x))
     in M.PixelSRGB r g b
