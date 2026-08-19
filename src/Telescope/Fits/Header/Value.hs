module Telescope.Fits.Header.Value where

import Data.Text (Text)
import Effectful
import Telescope.Asdf.NDArray
import Telescope.Data.Parser (expected, runParserAlts)


-- | `Value` datatype for discriminating valid FITS KEYWORD=VALUE types in an HDU.
data Value
  = Integer Int
  | Float Double
  | String Text
  | Logic LogicalConstant
  deriving (Show, Eq)


-- We can' go from Asdf.Value -> Fits.Value, which is only a subset, so we need to go directly from the ndarray
instance FromNDArray [Value] where
  fromNDArray :: (Parser :> es) => NDArrayData -> Eff es [Value]
  fromNDArray arr = do
    case arr.datatype of
      Bool8 -> fmap logical <$> fromNDArray @[Bool] arr
      Ucs4 _ -> fmap String <$> fromNDArray @[Text] arr
      Float32 -> fmap (Float . realToFrac) <$> fromNDArray @[Float] arr
      Float64 -> fmap Float <$> fromNDArray @[Double] arr
      _ -> fmap Integer <$> fromNDArray @[Int] arr
   where
    logical = \case
      True -> Logic T
      False -> Logic F


-- | Direct encoding of a `Bool` for parsing `Value`
data LogicalConstant = T | F
  deriving (Show, Eq)
