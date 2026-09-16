{-# LANGUAGE UndecidableInstances #-}

module Telescope.Data.DataCube where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array as M hiding (Dim1, Dimension, Dimensions, mapM, tail)
import Data.Massiv.Array qualified as M
import Data.Proxy
import GHC.TypeLits (natVal)
import Telescope.Data.Array (AxesIndex (..))
import Telescope.Data.Axes (Axes, Major (Row))
import Prelude hiding (head, tail)


newtype DataCube (as :: [Type]) f = DataCube
  { array :: Array D (IndexOf as) f
  }
deriving instance (Index (IndexOf as), Eq f) => Eq (DataCube as f)
deriving instance (Ragged L (IndexOf as) f, Show f) => Show (DataCube as f)


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
  let dims = fromIntegral $ natVal @(M.Dimensions (IndexOf (a : b : xs))) Proxy
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
  let dims = fromIntegral $ natVal @(M.Dimensions (IndexOf (a : b : c : xs))) Proxy
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
  let dims = fromIntegral $ natVal @(M.Dimensions (IndexOf (a : xs))) Proxy
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
  let dims = fromIntegral $ natVal @(M.Dimensions (IndexOf (a : xs))) Proxy
  (arr1, arr2) <- M.splitAtM (Dim dims) b arr
  pure (DataCube arr1, DataCube arr2)


dataCubeAxes :: (Index (IndexOf as), AxesIndex (IndexOf as)) => DataCube as f -> Axes Row
dataCubeAxes (DataCube arr) =
  let Sz ix = M.size arr
   in indexAxes ix


--------------------------------------------------------------------------------------

data Dimensions (axes :: [Type]) = Dimensions (AxesFor axes)
data Dimension (axis :: Type) = Dimension Int
  deriving (Show, Eq)


type family AxesFor (axes :: [Type]) :: Type where
  AxesFor [a, b, c, d, e] = (Int, Int, Int, Int, Int)
  AxesFor [a, b, c, d] = (Int, Int, Int, Int)
  AxesFor [a, b, c] = (Int, Int, Int)
  AxesFor [a, b] = (Int, Int)
  AxesFor '[a] = (Int)


class Uncons as where
  type Head as :: Type
  type Tail as :: Type
  uncons :: as -> (Head as, Tail as)
  head :: as -> Head as
  head as = let (h, _) = uncons as in h
  tail :: as -> Tail as
  tail as = let (_, t) = uncons as in t


instance Uncons (a, b) where
  type Head (a, b) = a
  type Tail (a, b) = b
  uncons (a, b) = (a, b)


instance Uncons (a, b, c) where
  type Head (a, b, c) = a
  type Tail (a, b, c) = (b, c)
  uncons (a, b, c) = (a, (b, c))


instance Uncons (a, b, c, d) where
  type Head (a, b, c, d) = a
  type Tail (a, b, c, d) = (b, c, d)
  uncons (a, b, c, d) = (a, (b, c, d))


class DimensionSize (axis :: Type) axes where
  dimensionSize :: Dimensions axes -> Dimension axis


instance {-# OVERLAPPABLE #-} (axes ~ AxesFor (a : xs), Head axes ~ Int, Uncons axes) => DimensionSize a (a : xs) where
  dimensionSize (Dimensions ds) = Dimension $ head ds


instance {-# OVERLAPS #-} (axes ~ AxesFor (x : xs), AxesFor xs ~ Tail axes, Uncons axes, DimensionSize a xs) => DimensionSize a (x : xs) where
  dimensionSize (Dimensions ds) =
    let ax :: Tail (AxesFor (x : xs)) = tail ds
     in dimensionSize @a @xs $ Dimensions ax


instance DimensionSize a '[a] where
  dimensionSize (Dimensions n) = Dimension n


data X
data Y
data Z


test :: IO ()
test = do
  let dxyz :: Dimensions [X, Y, Z] = _
  let dxy :: Dimensions [X, Y] = _
  let dx :: Dimensions '[X] = _
  let d0 :: Dimensions '[] = _
  let x = dimensionSize @X dxyz
  let y = dimensionSize @Y dxyz
  let z = dimensionSize @Z dxyz
  let x2 = dimensionSize @X dxy
  let y2 = dimensionSize @Y dxy
  let x3 = dimensionSize @X dx
  let x4 = dimensionSize @X d0
  print (x, y, z)
  pure ()
