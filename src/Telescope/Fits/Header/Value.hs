module Telescope.Fits.Header.Value where

import Data.Text (Text)
import Effectful
import Telescope.Asdf.NDArray
import Telescope.Data.Parser (expected, runParserAlts, tryParserEmpty, (<|>))


-- | `Value` datatype for discriminating valid FITS KEYWORD=VALUE types in an HDU.
data Value
  = Integer Int
  | Float Double
  | String Text
  | Logic LogicalConstant
  deriving (Show, Eq)


-- We can' go from Asdf.Value -> Fits.Value, it doesn't fit
instance FromNDArray [Value] where
  fromNDArray :: (Parser :> es) => NDArrayData -> Eff es [Value]
  fromNDArray dat = runParserAlts (expected "Fits Value" dat) $ do
    (fmap Integer <$> ints) <|> (fmap Float <$> floats) <|> (fmap String <$> strings)
   where
    ints = tryParserEmpty $ fromNDArray @[Int] dat
    floats = tryParserEmpty $ fromNDArray @[Double] dat
    strings = tryParserEmpty $ fromNDArray @[Text] dat


-- | Direct encoding of a `Bool` for parsing `Value`
data LogicalConstant = T | F
  deriving (Show, Eq)
