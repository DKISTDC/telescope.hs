module Telescope.Data.Axes where

import GHC.IsList (IsList (..))


type Axis = Int
newtype Axes a = Axes {axes :: [Axis]}
  deriving (Show, Eq)
data Row
data Column


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
