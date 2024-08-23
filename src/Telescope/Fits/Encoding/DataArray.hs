module Telescope.Fits.Encoding.DataArray where

import Control.Monad.Catch (MonadCatch)
import Data.Massiv.Array as M hiding (isEmpty, product)
import System.ByteOrder
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

>>> decodeDataArray @Ix2 @Float hdu.dataArray
Array D Seq (Sz (2 :. 3))
  [ [ 1.0, 2.0, 3.0 ]
  , [ 4.0, 5.0, 6.0 ]
  ]

This creates a delayed (D) array, which will postpone evaluation of cells until needed
-}
decodeDataArray :: forall ix a m. (MonadFail m, MonadThrow m, MonadCatch m) => (Index ix, AxesIndex ix, Prim a, BinaryValue a) => DataArray -> m (Array D ix a)
decodeDataArray DataArray{axes, rawData} = do
  decodeArrayOrder BigEndian (toRowMajor axes) rawData


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
   . (Source r a, Stream r Ix1 a, Size r, PutArray ix, Index ix, AxesIndex ix, BinaryValue a, Prim a, IsBitPix a)
  => Array r ix a
  -> DataArray
encodeDataArray arr =
  let axes = sizeAxes $ size arr
      bitpix = bitPix @a
      rawData = encodeArray arr -- O(n)
   in DataArray{bitpix, axes, rawData}


sizeAxes :: (AxesIndex ix, Index ix) => Sz ix -> Axes Column
sizeAxes (Sz ix) = toColumnMajor $ indexAxes ix
