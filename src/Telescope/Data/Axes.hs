{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.Axes where

import GHC.IsList (IsList (..))
import GHC.TypeLits


type Axis = Int


newtype Axes (a :: Major) = Axes {axes :: [Axis]}
  deriving (Show, Eq)


data Major = Row | Column


axesRowMajor :: [Axis] -> Axes Row
axesRowMajor = Axes


axesColumnMajor :: [Axis] -> Axes Column
axesColumnMajor = Axes


toRowMajor :: Axes Column -> Axes Row
toRowMajor (Axes as) = Axes (reverse as)


toColumnMajor :: Axes Row -> Axes Column
toColumnMajor (Axes as) = Axes (reverse as)


totalItems :: Axes a -> Int
totalItems (Axes as) = product as


instance IsList (Axes Row) where
  type Item (Axes Row) = Axis
  fromList = Axes
  toList (Axes ax) = ax


{- | Specify which numbered axis a type represents

> data X
> instance AxisOrder X where
>   axisN = 1
>
> data Y
> instance AxisOrder Y where
>   axisN = 2
-}
class AxisOrder ax where
  axisN :: Natural
