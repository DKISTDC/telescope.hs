{-# LANGUAGE UndecidableInstances #-}

module Telescope.Data.DataCube where

import Data.Kind
import Data.Massiv.Array as M hiding (mapM)
import Data.Proxy
import GHC.TypeLits (natVal)
import Telescope.Data.Array (AxesIndex (..))
import Telescope.Data.Axes (Axes, Major (Row))


-- Results ------------------------------------------------------------------------------

newtype DataCube (as :: [Type]) f = DataCube
  { array :: Array D (IndexOf as) f
  }


instance (Index (IndexOf as), Eq f) => Eq (DataCube as f) where
  DataCube arr == DataCube arr2 = arr == arr2


instance (Ragged L (IndexOf as) f, Show f) => Show (DataCube as f) where
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
  :: forall a as f
   . (Lower (IndexOf (a : as)) ~ IndexOf as, Index (IndexOf as), Index (IndexOf (a : as)))
  => DataCube (a : as) f
  -> [DataCube as f]
outerList (DataCube a) = foldOuterSlice row a
 where
  row :: Array D (IndexOf as) f -> [DataCube as f]
  row r = [DataCube r]


transposeMajor
  :: (IndexOf (a : b : xs) ~ IndexOf (b : a : xs), Index (Lower (IndexOf (b : a : xs))), Index (IndexOf (b : a : xs)))
  => DataCube (a : b : xs) f
  -> DataCube (b : a : xs) f
transposeMajor (DataCube arr) = DataCube $ transposeInner arr


transposeMinor4
  :: DataCube [a, b, c, d] f
  -> DataCube [a, b, d, c] f
transposeMinor4 (DataCube arr) = DataCube $ transposeOuter arr


transposeMinor3
  :: DataCube [a, b, c] f
  -> DataCube [a, c, b] f
transposeMinor3 (DataCube arr) = DataCube $ transposeOuter arr


-- Slice along the 1st major dimension
sliceM0
  :: ( Lower (IndexOf (a : xs)) ~ IndexOf xs
     , Index (IndexOf xs)
     , Index (IndexOf (a : xs))
     )
  => Int
  -> DataCube (a : xs) f
  -> DataCube xs f
sliceM0 a (DataCube arr) = DataCube (arr !> a)


-- Slice along the 2nd major dimension
sliceM1
  :: forall a b xs f
   . ( Lower (IndexOf (a : b : xs)) ~ IndexOf (a : xs)
     , Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     )
  => Int
  -> DataCube (a : b : xs) f
  -> DataCube (a : xs) f
sliceM1 b (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 1), b)


-- Slice along the 3rd major dimension
sliceM2
  :: forall a b c xs f
   . ( Lower (IndexOf (a : b : c : xs)) ~ IndexOf (a : b : xs)
     , Index (IndexOf (a : b : xs))
     , Index (IndexOf (a : b : c : xs))
     )
  => Int
  -> DataCube (a : b : c : xs) f
  -> DataCube (a : b : xs) f
sliceM2 c (DataCube arr) =
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : b : c : xs))) Proxy
   in DataCube $ arr <!> (Dim (dims - 2), c)


splitM0
  :: forall a xs f m
   . ( Index (IndexOf (a : xs))
     , MonadThrow m
     )
  => Int
  -> DataCube (a : xs) f
  -> m (DataCube (a : xs) f, DataCube (a : xs) f)
splitM0 a (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) a arr
  pure (DataCube arr1, DataCube arr2)


splitM1
  :: forall a b xs f m
   . ( Index (IndexOf (a : xs))
     , Index (IndexOf (a : b : xs))
     , MonadThrow m
     )
  => Int
  -> DataCube (a : b : xs) f
  -> m (DataCube (a : b : xs) f, DataCube (a : b : xs) f)
splitM1 b (DataCube arr) = do
  let dims = fromIntegral $ natVal @(Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (DataCube arr1, DataCube arr2)


dataCubeAxes :: (Index (IndexOf as), AxesIndex (IndexOf as)) => DataCube as f -> Axes Row
dataCubeAxes (DataCube arr) =
  let Sz ix = M.size arr
   in indexAxes ix
