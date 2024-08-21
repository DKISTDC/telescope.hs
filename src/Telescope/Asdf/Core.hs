{-# LANGUAGE OverloadedLists #-}

module Telescope.Asdf.Core where

import Data.String (fromString)
import Data.Text (Text)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_telescope (version)
import Telescope.Asdf.Class
import Telescope.Asdf.Error (expected)
import Telescope.Asdf.Node


-- VOUnit https://www.ivoa.net/documents/VOUnits/20231215/REC-VOUnits-1.1.html
-- >> Unrecognised units should be accepted by parsers, as long as they are parsed giving preference to the syntaxes and prefixes described here.
data Unit
  = Count
  | Pixel
  | Unit Text


instance ToAsdf Unit where
  schema = "!unit/unit-1.5.0"
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
  deriving (Generic)
instance ToAsdf Quantity where
  schema = "!unit/quantity-1.5.0"
instance FromAsdf Quantity


-- newtype Points = Points [[Double]]
--
--
-- instance ToAsdf Points where
--   schema = "unit/quantity-1.1.0"
--   toValue (Points ds) = do
--     toValue $ Quantity Pixel (NDArray $ toNDArray ds)
--
--
-- instance FromAsdf Points where
--   parseValue v = do
--     q <- parseValue v :: Parser Quantity
--     ps <- parseNDArray q.value
--     pure $ Points ps
--
--
-- newtype Points' = Points' (Array D Ix1 Double)
--
--
-- instance ToAsdf Points' where
--   schema = "unit/quantity-1.1.0"
--   toValue (Points' ds) = do
--     toValue $ Quantity Pixel (NDArray $ toNDArray ds)
--
--
-- instance FromAsdf Points' where
--   parseValue v = do
--     q <- parseValue v :: Parser Quantity
--     ps <- parseNDArray q.value
--     pure $ Points' ps

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

-- TEST: this is probably index dependent
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
      BoundingBox <$> parseNode n1 <*> parseNode n2
    node -> fail $ expected "BoundingBox" node


data Software = Software
  { author :: Maybe Text
  , homepage :: Maybe Text
  , name :: Text
  , version :: Text
  }
  deriving (Show, Eq, Generic, FromAsdf)
instance ToAsdf Software where
  schema = "!core/software-1.0.0"


telescopeSoftware :: Software
telescopeSoftware =
  Software
    { author = Just "DKIST Data Center"
    , homepage = Just "https://github.com/dkistdc/telescope.hs"
    , name = "telescope.hs"
    , version = fromString $ showVersion version
    }


-- allows "additional properties"...
data Asdf = Asdf
  { history :: History
  , library :: Software
  , tree :: Object
  }
instance ToAsdf Asdf where
  schema = "!core/asdf-1.1.0"
  toValue a =
    -- these two required fields are first, then merge keys from the tree
    Object $
      [ ("asdf_library", toNode a.library)
      , ("history", toNode a.history)
      ]
        <> a.tree
instance FromAsdf Asdf where
  parseValue = \case
    Object o -> do
      library <- o .: "asdf_library"
      history <- o .: "history"
      let tree = filter (not . isLibraryField) o
      pure $ Asdf{history, library, tree}
    val -> fail $ expected "Asdf" val
   where
    isLibraryField ("asdf_library", _) = True
    isLibraryField ("history", _) = True
    isLibraryField _ = False


-- coreTag :: Text -> SchemaTag
-- coreTag t = SchemaTag $ Just $

data History = History
  { extensions :: [ExtensionMetadata]
  }
  deriving (Show, Generic, FromAsdf, ToAsdf)


data ExtensionMetadata = ExtensionMetadata
  { extension_class :: Text
  , software :: Software
  }
  deriving (Show, Generic, FromAsdf)
instance ToAsdf ExtensionMetadata where
  schema = "!core/extension_metadata-1.0.0"
