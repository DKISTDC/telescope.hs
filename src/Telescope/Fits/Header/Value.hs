module Telescope.Fits.Header.Value where

import Data.Text (Text)


-- | `Value` datatype for discriminating valid FITS KEYWORD=VALUE types in an HDU.
data Value
  = Integer Int
  | Float Double
  | String Text
  | Logic LogicalConstant
  deriving (Show, Eq)


-- | Direct encoding of a `Bool` for parsing `Value`
data LogicalConstant = T | F
  deriving (Show, Eq)
