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


{- | VOUnit: https://www.ivoa.net/documents/VOUnits/20231215/REC-VOUnits-1.1.html
 -
Unrecognised units should be accepted by parsers, as long as they are parsed giving preference to the syntaxes and prefixes described here.
-}
data Unit
  = Count
  | Pixel
  | Degrees
  | Nanometers
  | Meters
  | Kilometers
  | Arcseconds
  | Seconds
  | Unit Text
  deriving (Eq)


instance ToAsdf Unit where
  schema _ = "!unit/unit-1.0.0"
  toValue = \case
    Count -> "count"
    Pixel -> "pixel"
    Degrees -> "deg"
    Nanometers -> "nm"
    Kilometers -> "km"
    Seconds -> "s"
    Arcseconds -> "arcsec"
    Meters -> "m"
    (Unit t) -> String t
instance FromAsdf Unit where
  parseValue = \case
    String "count" -> pure Count
    String "deg" -> pure Degrees
    String "pixel" -> pure Pixel
    String "pix" -> pure Pixel
    String "nm" -> pure Nanometers
    String "km" -> pure Kilometers
    String "m" -> pure Meters
    String "s" -> pure Seconds
    String "arcsec" -> pure Arcseconds
    String t -> pure $ Unit t
    val -> expected "String" val


-- | Tag a value with a 'Unit'
data Quantity = Quantity
  { unit :: Unit
  , value :: Value
  }
  deriving (Generic)


instance ToAsdf Quantity where
  schema _ = "!unit/quantity-1.2.0"
instance FromAsdf Quantity


--
-- -- TEST: this is probably index dependent
-- data BoundingBox = BoundingBox Double Double
--
--
-- instance ToAsdf BoundingBox where
--   toValue (BoundingBox a b) =
--     Array
--       [ toNode $ Quantity Pixel (toValue a)
--       , toNode $ Quantity Pixel (toValue b)
--       ]
--
--
-- instance FromAsdf BoundingBox where
--   parseValue = \case
--     Array [n1, n2] -> do
--       BoundingBox <$> parseNode n1 <*> parseNode n2
--     node -> expected "BoundingBox" node

-- | Required Software node at the top-level
data Software = Software
  { author :: Maybe Text
  , homepage :: Maybe Text
  , name :: Text
  , version :: Text
  }
  deriving (Show, Eq, Generic, FromAsdf)


instance ToAsdf Software where
  schema _ = "!core/software-1.0.0"


-- | Root ASDF node
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
