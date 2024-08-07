module Telescope.Fits.Encoding.DataArray where

import Control.Monad.Catch
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array as M hiding (isEmpty, product)
import Data.Proxy
import GHC.Int
import System.ByteOrder (ByteOrder (BigEndian))
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
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
decodeDataArray :: forall ix a m. (MonadThrow m, MonadUnliftIO m, Manifest D a) => (Index ix, AxesIndex ix, Prim a, BinaryValue a) => DataArray -> m (Array D ix a)
decodeDataArray DataArray{axes, rawData} = do
  decodeArrayOrder BigEndian (toRowMajor axes) rawData


-- | Decode Axes as a delayed stream 1d vector
getAxesVector :: Get a -> Axes Column -> Get (Vector DS a)
getAxesVector gt as = do
  sreplicateM (Sz1 (totalPix as)) gt


----- ENCODING -----------------------------------------------------------------

{- | Encode an 'Array' to a 'DataArray'

>>> encodeImage array
DataArray:
  data: 48 bytes
  dimensions:
    format: Int64
    axes: [3,2]
-}
encodeDataArray
  :: forall r ix a
   . (Source r a, Stream r Ix1 a, Size r, PutArray ix, Index ix, AxesIndex ix, BinaryValue a, IsBitPix a, Prim a)
  => Array r ix a
  -> DataArray
encodeDataArray arr =
  let axes = sizeAxes $ size arr
      bitpix = bitPixFormat @a Proxy
      rawData = encodeArray arr -- O(n)
   in DataArray{bitpix, axes, rawData}


-- | The total number of pixels to read from the input ByteString
totalPix :: Axes a -> Int
totalPix (Axes as) = product as


sizeAxes :: (AxesIndex ix, Index ix) => Sz ix -> Axes Column
sizeAxes (Sz ix) = toColumnMajor $ indexAxes ix


class IsBitPix a where
  bitPixFormat :: Proxy a -> BitPix


instance IsBitPix Int8 where
  bitPixFormat _ = BPInt8
instance IsBitPix Int16 where
  bitPixFormat _ = BPInt16
instance IsBitPix Int32 where
  bitPixFormat _ = BPInt32
instance IsBitPix Int64 where
  bitPixFormat _ = BPInt64
instance IsBitPix Float where
  bitPixFormat _ = BPFloat
instance IsBitPix Double where
  bitPixFormat _ = BPDouble
