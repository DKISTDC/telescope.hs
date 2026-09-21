{-# LANGUAGE UndecidableInstances #-}

module Telescope.Data.DataCube
  ( DataCube (..)
  , outerList
  , transposeMajor
  , transposeMinor4
  , transposeMinor3
  , sliceM0
  , sliceM1
  , sliceM2
  , splitM0
  , splitM1
  , dataCubeAxes
  , Dimensions (..)
  , Dimension (..)
  , DimensionSize (..)
  , HasIndex (..)
  , IxN ((:>))
  , Ix2 ((:.))
  , Sz (..)
  )
where

import Data.Kind
import Data.Massiv.Array as M hiding (Dim1, Dimension, Dimensions, mapM, tail)
import Data.Massiv.Array qualified as M
import Data.Proxy
import GHC.TypeNats
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

data Dimensions (axes :: [Type]) = Dimensions (Sz (IndexOf axes))
data Dimension (axis :: Type) = Dimension Int
  deriving (Show, Eq)


-- type family Sizes (axes :: [Type]) :: Type where
--   Sizes '[a] = (Int)
--   Sizes (x ': xs) = (Int, Sizes xs)
--   Sizes '[] = TypeError ('Text "Type not found in axis list")

class DimensionSize (axis :: Type) (axes :: [Type]) where
  dimensionSize :: Dimensions axes -> Dimension axis


instance DimensionSize a '[a, b, c, d] where
  dimensionSize (Dimensions (Sz (a :> _))) = Dimension a


instance DimensionSize b '[a, b, c, d] where
  dimensionSize (Dimensions (Sz (_ :> b :> _))) = Dimension b


instance DimensionSize c '[a, b, c, d] where
  dimensionSize (Dimensions (Sz (_ :> _ :> c :. _))) = Dimension c


instance DimensionSize d '[a, b, c, d] where
  dimensionSize (Dimensions (Sz (_ :> _ :> _ :. d))) = Dimension d


instance DimensionSize a '[a, b, c] where
  dimensionSize (Dimensions (Sz (a :> _ :. _))) = Dimension a


instance DimensionSize b '[a, b, c] where
  dimensionSize (Dimensions (Sz (_ :> b :. _))) = Dimension b


instance DimensionSize c '[a, b, c] where
  dimensionSize (Dimensions (Sz (_ :> _ :. c))) = Dimension c


instance DimensionSize a '[a, b] where
  dimensionSize (Dimensions (Sz (a :. _))) = Dimension a


instance DimensionSize a '[b, a] where
  dimensionSize (Dimensions (Sz (_ :. b))) = Dimension b


instance DimensionSize a '[a] where
  dimensionSize (Dimensions (Sz1 n)) = Dimension n

-- data X
-- data Y
-- data Z
-- data A
--
--
-- test :: IO ()
-- test = do
--   let dxyz :: Dimensions [X, Y, Z] = undefined
--   let dxy :: Dimensions [X, Y] = undefined
--   let dx :: Dimensions '[X] = undefined
--   let d0 :: Dimensions '[] = undefined
--   let a = dimensionSize @A dxyz
--   let x = dimensionSize @X dxyz
--   let y = dimensionSize @Y dxyz
--   let z = dimensionSize @Z dxyz
--   let x2 = dimensionSize @X dxy
--   let y2 = dimensionSize @Y dxy
--   let x3 = dimensionSize @X dx
--   let x4 = dimensionSize @X d0
--   let zz = dimensionSize @Z dxy
--   pure ()
