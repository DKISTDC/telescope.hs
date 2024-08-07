module Telescope.Data.Array where

import Control.Monad.Catch
import Data.Binary.Get (ByteOffset, runGetOrFail)
import Data.Binary.Put (Put, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, Comp (..), D, Index, Ix1, Ix2 (..), Ix3, Ix4, Ix5, IxN (..), Lower, Manifest, MonadUnliftIO, Prim, Source, Stream, Sz (..), Vector)
import Data.Massiv.Array qualified as M
import Data.Word (Word8)
import System.ByteOrder (ByteOrder (..))
import Telescope.Data.Axes
import Telescope.Data.Binary


{- | Decode binary data into an Array of arbitrary dimensions 'ix' by specifying the 'Axes' 'Row'

>>> decodeArray @Ix2 @Float [3, 2] input
Array P Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]
-}
decodeArray
  :: forall ix a m
   . (AxesIndex ix, Prim a, BinaryValue a, Index ix, MonadThrow m, MonadUnliftIO m, Manifest D a)
  => Axes Row
  -> BS.ByteString
  -> m (Array D ix a)
decodeArray = decodeArrayOrder BigEndian


{- | Encode an Array as a Lazy ByteString based on the type of the element 'a'

>>> myArray = decodeArray @Ix2 @Float BPFloat [3, 2] input
>>> output = encodeArray myArray
-}
encodeArray
  :: (Source r a, Stream r Ix1 a, PutArray ix, Index ix, BinaryValue a, Prim a)
  => Array r ix a
  -> BS.ByteString
encodeArray = BL.toStrict . runPut . putArray BigEndian


{- | Decode binary data into an Array of arbitrary dimensions 'ix' by specifying the 'Axes' 'Row' and the byte order

>>> decodeArrayOrder @Ix2 @Float BigEndian [3, 2] input
Array P Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]
-}
decodeArrayOrder
  :: forall ix a m
   . (AxesIndex ix, Prim a, BinaryValue a, Index ix, MonadThrow m, MonadUnliftIO m, Manifest D a)
  => ByteOrder
  -> Axes Row
  -> BS.ByteString
  -> m (Array D ix a)
decodeArrayOrder bo as inp = do
  v <- decodeVector @a Par bo inp
  fromVector as v


-- | Decode binary data into as a 1d Vector ~ (Array r Ix1 a)
decodeVector
  :: forall a m
   . (BinaryValue a, Manifest D a, MonadThrow m, MonadUnliftIO m)
  => Comp
  -> ByteOrder
  -> BS.ByteString
  -> m (Vector D a)
decodeVector c bo inp = do
  let v = parseWordVector inp
  M.generateArray c (arraySize v) (valueAt v)
 where
  numBytes = byteSize @a

  arraySize v =
    let Sz s = M.size v
     in Sz $ s `div` numBytes

  valueAt :: Vector D Word8 -> Ix1 -> m a
  valueAt v ix = do
    let ws = wordsAt v ix
    fromWords ws

  wordsAt :: Vector D Word8 -> Ix1 -> [Word8]
  wordsAt v ix =
    M.toList $ M.slice (ix * numBytes) (Sz numBytes) v

  parseWordVector :: BS.ByteString -> Vector D Word8
  parseWordVector = M.fromByteString c

  fromWords :: [Word8] -> m a
  fromWords ws =
    case runGetOrFail (get bo) (BL.pack ws) of
      Left (ip, byts, e) -> throwM $ BinaryParseError byts (e <> " " <> show ip <> show ws)
      Right (_, _, a) -> pure a


-- | Resize a Vector into an Array
fromVector
  :: forall ix a m
   . (AxesIndex ix, Index ix, Prim a, MonadThrow m)
  => Axes Row
  -> Vector D a
  -> m (Array D ix a)
fromVector as v = do
  ix <- axesIndex as
  M.resizeM (Sz ix) v


data ArrayError
  = BinaryParseError !ByteOffset String
  | AxesMismatch !(Axes Row)
  deriving (Show, Exception)


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


class PutArray ix where
  putArray
    :: (Source r a, Stream r Ix1 a, BinaryValue a, Prim a)
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
