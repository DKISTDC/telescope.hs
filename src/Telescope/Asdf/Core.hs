{-# LANGUAGE OverloadedLists #-}

module Telescope.Asdf.Core where

import Data.String (fromString)
import Data.Text (Text)
import Data.Version (showVersion)
import Effectful
import Effectful.Error.Static
import GHC.Generics (Generic)
import Paths_telescope (version)
import Telescope.Asdf.Class
import Telescope.Asdf.Error (AsdfError (..))
import Telescope.Asdf.Node
import Telescope.Data.Parser (expected)


-- VOUnit https://www.ivoa.net/documents/VOUnits/20231215/REC-VOUnits-1.1.html
-- >> Unrecognised units should be accepted by parsers, as long as they are parsed giving preference to the syntaxes and prefixes described here.
data Unit
  = Count
  | Pixel
  | Degrees
  | Unit Text
  deriving (Eq)


instance ToAsdf Unit where
  schema _ = "!unit/unit-1.0.0"
  toValue = \case
    Count -> "count"
    Pixel -> "pixel"
    Degrees -> "deg"
    (Unit t) -> String t
instance FromAsdf Unit where
  parseValue = \case
    String "count" -> pure Count
    String "deg" -> pure Degrees
    String "pixel" -> pure Pixel
    String "pix" -> pure Pixel
    String t -> pure $ Unit t
    val -> expected "String" val


-- it seems silly to parse into these. this is dynamic typing!
data Quantity = Quantity
  { unit :: Unit
  , value :: Value
  }
  deriving (Generic)
instance ToAsdf Quantity where
  schema _ = "!unit/quantity-1.1.0"
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
    node -> expected "BoundingBox" node


data Software = Software
  { author :: Maybe Text
  , homepage :: Maybe Text
  , name :: Text
  , version :: Text
  }
  deriving (Show, Eq, Generic, FromAsdf)
instance ToAsdf Software where
  schema _ = "!core/software-1.0.0"


-- allows "additional properties"...
data Asdf = Asdf
  { history :: History
  , library :: Software
  , tree :: Tree
  }
instance ToAsdf Asdf where
  schema _ = "!core/asdf-1.1.0"
  toValue a =
    let Tree tree = a.tree
     in -- these two required fields are first, then merge keys from the tree
        Object $
          [ ("asdf_library", toNode a.library)
          , ("history", toNode a.history)
          ]
            <> tree
instance FromAsdf Asdf where
  parseValue = \case
    Object o -> do
      library <- o .: "asdf_library"
      history <- o .: "history"
      let tree = Tree $ filter (not . isLibraryField) o
      pure $ Asdf{history, library, tree}
    val -> expected "Asdf" val
   where
    isLibraryField ("asdf_library", _) = True
    isLibraryField ("history", _) = True
    isLibraryField _ = False


-- | Convert any ToAsdf into a raw Asdf document
toAsdfDoc :: (ToAsdf a, Error AsdfError :> es) => a -> Eff es Asdf
toAsdfDoc a =
  case toValue a of
    Object o -> do
      let history = History []
      let library = telescopeLibrary
      pure $ Asdf{history, library, tree = Tree o}
    value -> throwError $ EncodeError $ "Expected Top-level Tree Object, but got: " ++ show value
 where
  telescopeLibrary :: Software
  telescopeLibrary =
    Software
      { author = Just "DKIST Data Center"
      , homepage = Just "https://github.com/dkistdc/telescope.hs"
      , name = "telescope.hs"
      , version = fromString $ showVersion version
      }


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
  schema _ = "!core/extension_metadata-1.0.0"
