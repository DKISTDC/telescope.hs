module Telescope.Asdf.Core where

import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.ByteOrder (ByteOrder (..))
import Telescope.Asdf.Node
import Telescope.Fits.Types (Axes, Row)


-- VOUnit https://www.ivoa.net/documents/VOUnits/20231215/REC-VOUnits-1.1.html
-- >> Unrecognised units should be accepted by parsers, as long as they are parsed giving preference to the syntaxes and prefixes described here.
-- umm...............
-- ..................
-- then do these transform themsleves too?
data Unit
  = Count
  | Pixel
  | Unit Text


instance ToAsdf Unit where
  type Schema Unit = "unit/unit-1.5.0"
  toValue = \case
    Count -> "count"
    Pixel -> "pixel"
    (Unit t) -> String t
  fromValue = \case
    String "count" -> pure Count
    String "pixel" -> pure Pixel
    String "pix" -> pure Pixel
    String t -> pure $ Unit t
    val -> fail $ "Expected String, but got: " ++ show val


data Quantity = Quantity
  { unit :: Unit
  , value :: Value
  }


instance ToAsdf Quantity where
  type Schema Quantity = "unit/quantity-1.5.0"
  toValue q =
    Object
      [ ("unit", toNode q.unit)
      , ("value", Node Nothing q.value)
      ]
  fromValue = \case
    Object o -> do
      unit <- o .: "unit"
      Node _ value <- field "value" o
      pure $ Quantity{unit, value}
    val -> fail $ "Expected Object, but got: " ++ show val


data NDArray = NDArray
  { source :: ByteString
  , byteorder :: ByteOrder
  , datatype :: DataType
  , shape :: Axes Row
  }
data DataType = Float64


-- what if this is two classes?
-- Schema
-- ToAsdf
instance ToAsdf NDArray where
  type Schema NDArray = "core/ndarray-1.5.0"
  toValue a =
    Object
      [ ("source", Node Nothing (Binary a.source))
      , ("byteorder", Node Nothing a.byteorder)
      , ("datatype", Node Nothing a.datatype)
      , ("shape", Node Nothing a.shape)
      ]
  fromValue = \case
    Object o -> do
      unit <- o .: "unit"
      Node _ value <- field "value" o
      pure $ Quantity{unit, value}
    val -> fail $ "Expected Object, but got: " ++ show val

--  points:
-- - !unit/quantity-1.1.0
--   unit: !unit/unit-1.0.0 pixel
--   value: !core/ndarray-1.0.0
--     source: 260
--     datatype: float64
--     byteorder: little
--     shape: [85]
