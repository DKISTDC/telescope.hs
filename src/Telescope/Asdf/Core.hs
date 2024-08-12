{-# LANGUAGE OverloadedLists #-}

module Telescope.Asdf.Core where

import Data.Massiv.Array
import Data.Text (Text)
import Telescope.Asdf.Class
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser


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
  parseValue = \case
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
  parseValue = \case
    Object o -> do
      unit <- o .: "unit"
      value <- o .: "value"
      pure $ Quantity{unit, value}
    val -> fail $ expected "Quantity" val


-- instance ToAsdf NDArray where
--   schema = "core/ndarray-1.5.0"
--   toValue a =
--     Object
--       [ ("source", toNode a.source)
--       , ("byteorder", toNode a.byteorder)
--       , ("datatype", toNode a.datatype)
--       , ("shape", toNode a.shape)
--       ]
--
--
-- instance FromAsdf NDArray where
--   parseValue = \case
--     Object o -> do
--       sc <- o .: "source"
--       bo <- o .: "byteorder"
--       dt <- o .: "datatype"
--       sh <- o .: "shape"
--       pure $ NDArray sc bo dt sh
--     val -> fail $ expected "NDArray" val

-- instance ToAsdf DataType where
--   toValue Float64 = String "Float64"
-- instance FromAsdf DataType where
--   parseValue = \case
--     String "Float64" -> pure Float64
--     node -> fail $ expected "DataType" node

-- points are really just an array, encoded in the ndarray
newtype Points = Points [[Double]]


instance ToAsdf Points where
  schema = "unit/quantity-1.1.0"
  toValue (Points ds) = do
    toValue $ Quantity Pixel (NDArray $ toNDArray ds)


instance FromAsdf Points where
  parseValue v = do
    q <- parseValue v :: Parser Quantity
    ps <- parseNDArray q.value
    pure $ Points ps


newtype Points' = Points' (Array D Ix1 Double)


instance ToAsdf Points' where
  schema = "unit/quantity-1.1.0"
  toValue (Points' ds) = do
    toValue $ Quantity Pixel (NDArray $ toNDArray ds)


instance FromAsdf Points' where
  parseValue v = do
    q <- parseValue v :: Parser Quantity
    ps <- parseNDArray q.value
    pure $ Points' ps


-- TEST: this is probably index dependnent
data BoundingBox = BoundingBox Double Double


instance ToAsdf BoundingBox where
  toValue (BoundingBox a b) =
    Array
      [ toNode $ Quantity Pixel (toValue a)
      , toNode $ Quantity Pixel (toValue b)
      ]


instance FromAsdf BoundingBox where
  parseValue = \case
    Array [n1, n2] -> do
      BoundingBox <$> fromNode n1 <*> fromNode n2
    node -> fail $ expected "BoundingBox" node

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
--
--
