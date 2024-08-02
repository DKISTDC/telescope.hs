module Telescope.Asdf.Core where

import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import GHC.ByteOrder (ByteOrder (..))
import Telescope.Asdf.Node
import Telescope.Fits.Types (Axes (..), Row)


-- VOUnit https://www.ivoa.net/documents/VOUnits/20231215/REC-VOUnits-1.1.html
-- >> Unrecognised units should be accepted by parsers, as long as they are parsed giving preference to the syntaxes and prefixes described here.
data Unit
  = Count
  | Pixel
  | Unit Text


instance ToAsdf Unit where
  schema = "unit/unit-1.5.0"
  toValue = \case
    Count -> "count"
    Pixel -> "pixel"
    (Unit t) -> String t
instance FromAsdf Unit where
  fromValue = \case
    String "count" -> pure Count
    String "pixel" -> pure Pixel
    String "pix" -> pure Pixel
    String t -> pure $ Unit t
    val -> fail $ "Expected String, but got: " ++ show val


-- it seems silly to parse into these. this is dynamic typing!
data Quantity = Quantity
  { unit :: Unit
  , value :: Value
  }


instance ToAsdf Quantity where
  schema = "unit/quantity-1.5.0"
  toValue q =
    Object
      [ ("unit", toNode q.unit)
      , ("value", Node mempty q.value)
      ]
instance FromAsdf Quantity where
  fromValue = \case
    Object o -> do
      unit <- o .: "unit"
      value <- o .: "value"
      pure $ Quantity{unit, value}
    val -> fail $ expected "Quantity" val


data NDArray = NDArray
  { source :: ByteString
  , byteorder :: ByteOrder
  , datatype :: DataType
  , shape :: Axes Row
  }


instance ToAsdf NDArray where
  schema = "core/ndarray-1.5.0"
  toValue a =
    Object
      [ ("source", toNode a.source)
      , ("byteorder", toNode a.byteorder)
      , ("datatype", toNode a.datatype)
      , ("shape", toNode a.shape)
      ]


instance FromAsdf NDArray where
  fromValue = \case
    Object o -> do
      sc <- o .: "source"
      bo <- o .: "byteorder"
      dt <- o .: "datatype"
      sh <- o .: "shape"
      pure $ NDArray sc bo dt sh
    val -> fail $ expected "NDArray" val


data DataType = Float64


instance ToAsdf DataType where
  toValue Float64 = String "Float64"
instance FromAsdf DataType where
  fromValue = \case
    String "Float64" -> pure Float64
    node -> fail $ expected "DataType" node


-- points are really just an array, encoded in the ndarray
newtype Points = Points [Double]


instance ToAsdf Points where
  schema = "unit/quantity-1.1.0"
  toValue (Points ds) = _

-- instance ToAsdf Points where
--   schema = "unit/quantity-1.1.0"
--   toValue (Points ds) = _

-- we could have embedded data as a class?
-- we are really just specifying how type x

--  points:
-- - !unit/quantity-1.1.0
--   unit: !unit/unit-1.0.0 pixel
--   value: !core/ndarray-1.0.0
--     source: 260
--     datatype: float64
--     byteorder: little
