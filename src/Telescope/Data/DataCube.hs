{-# LANGUAGE UndecidableInstances #-}

module Telescope.Data.DataCube where

import Data.Kind
import Data.Massiv.Array as M hiding (mapM)
import Data.Proxy
import GHC.TypeLits (natVal)
import Telescope.Data.Array (AxesIndex (..))
import Telescope.Data.Axes (Axes, Major (Row))


-- Results ------------------------------------------------------------------------------

newtype DataCube (as :: [Type]) = DataCube
  { array :: Array D (IndexOf as) Double
  }


instance (Index (IndexOf as)) => Eq (DataCube as) where
  DataCube arr == DataCube arr2 = arr == arr2


instance (Ragged L (IndexOf as) Double) => Show (DataCube as) where
  show (DataCube a) = show a


class HasIndex (as :: [Type]) where
  type IndexOf as :: Type


instance HasIndex '[] where
  type IndexOf '[] = Ix0
instance HasIndex '[a] where
  type IndexOf '[a] = Ix1
instance HasIndex '[a, b] where
  type IndexOf '[a, b] = Ix2
instance HasIndex '[a, b, c] where
  type IndexOf '[a, b, c] = Ix3
instance HasIndex '[a, b, c, d] where
  type IndexOf '[a, b, c, d] = Ix4
instance HasIndex '[a, b, c, d, e] where
  type IndexOf '[a, b, c, d, e] = Ix5


outerList
  :: forall a as
   . (Lower (IndexOf (a : as)) ~ IndexOf as, Index (IndexOf as), Index (IndexOf (a : as)))
  => DataCube (a : as)
  -> [DataCube as]
outerList (DataCube a) = foldOuterSlice row a
 where
  row :: Array D (IndexOf as) Double -> [DataCube as]
  row r = [DataCube r]


transposeMajor
  :: (IndexOf (a : b : xs) ~ IndexOf (b : a : xs), Index (Lower (IndexOf (b : a : xs))), Index (IndexOf (b : a : xs)))
  => DataCube (a : b : xs)
  -> DataCube (b : a : xs)
transposeMajor (DataCube arr) = DataCube $ transposeInner arr


transposeMinor4
  :: DataCube [a, b, c, d]
  -> DataCube [a, b, d, c]
transposeMinor4 (DataCube arr) = DataCube $ transposeOuter arr


transposeMinor3
  :: DataCube [a, b, c]
  -> DataCube [a, c, b]
transposeMinor3 (DataCube arr) = DataCube $ transposeOuter arr


-- Slice along the 1st major dimension
sliceM0
  :: ( Lower (IndexOf (a : xs)) ~ IndexOf xs
     , Index (IndexOf xs)
     , Index (IndexOf (a : xs))
     )
  => Int
  -> DataCube (a : xs)
  -> DataCube xs
sliceM0 a (DataCube arr) = DataCube (arr !> a)


-- Slice along the 2nd major dimension
sliceM1
  :: forall a b xs
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     )
  => Int
  -> DataCube (a : b : xs)
  -> DataCube (a : xs)
sliceM1 b (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 1), b)


-- Slice along the 3rd major dimension
sliceM2
  :: forall a b c xs
   . ( Lower (IndexOf (a : b : c : xs)) ~ IndexOf (a : b : xs)
     , Index (IndexOf (a : b : xs))
     , Index (IndexOf (a : b : c : xs))
     )
  => Int
  -> DataCube (a : b : c : xs)
  -> DataCube (a : b : xs)
sliceM2 c (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : c : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 2), c)


splitM0
  :: forall a xs m
   . ( Index (IndexOf (a : xs))
     , MonadThrow m
     )
  => Int
  -> DataCube (a : xs)
  -> m (DataCube (a : xs), DataCube (a : xs))
splitM0 a (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) a arr
  pure (DataCube arr1, DataCube arr2)


splitM1
  :: forall a b xs m
   . ( Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     , MonadThrow m
     )
  => Int
  -> DataCube (a : b : xs)
  -> m (DataCube (a : b : xs), DataCube (a : b : xs))
splitM1 b (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (DataCube arr1, DataCube arr2)


dataCubeAxes :: (Index (IndexOf as), AxesIndex (IndexOf as)) => DataCube as -> Axes Row
dataCubeAxes (DataCube arr) =
  let Sz ix = M.size arr
   in indexAxes ix
