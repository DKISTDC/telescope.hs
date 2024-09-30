module Telescope.Asdf.Node where

import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Telescope.Asdf.NDArray.Types
import Telescope.Data.Parser
import Text.Read (readMaybe)


-- | Specify a schema using 'schema' from 'ToAsdf'
newtype SchemaTag = SchemaTag (Maybe Text)
  deriving newtype (Monoid, Semigroup, Eq)


instance Show SchemaTag where
  show (SchemaTag Nothing) = ""
  show (SchemaTag (Just t)) = unpack t ++ ":"


schemaTag :: String -> SchemaTag
schemaTag = SchemaTag . Just . pack


instance IsString SchemaTag where
  fromString s = SchemaTag (Just $ pack s)


data Node = Node
  { schema :: SchemaTag
  , anchor :: Maybe Anchor
  , value :: Value
  }
  deriving (Eq)
instance Show Node where
  show (Node st _ v) = show st ++ show v
instance IsString Node where
  fromString s = Node mempty Nothing $ String $ pack s


-- We can't use Aeson's Value, because it doesn't support tags, binary data, or references
data Value
  = Bool !Bool
  | Number !Scientific
  | Integer !Integer
  | String !Text
  | -- | RawBinary !ByteString
    NDArray !NDArrayData
  | Array ![Node]
  | Object !Object
  | Reference !JSONReference
  | Alias !Anchor
  | Null
  deriving (Show, Eq)
instance IsString Value where
  fromString = String . pack
instance Semigroup Value where
  String a <> String b = String $ a <> b
  Array as <> Array bs = Array $ as <> bs
  Object as <> Object bs = Object $ as <> bs
  Null <> b = b
  a <> Null = a
  a <> _ = a
instance Monoid Value where
  mempty = Null


type Key = Text
type Object = [(Key, Node)]


-- | Makes a node from a value
fromValue :: Value -> Node
fromValue = Node mempty Nothing


-- | Root Object with all anchors resolved
newtype Tree = Tree Object
  deriving newtype (Semigroup, Monoid, Show)


-- always $ref: uri#path
data JSONReference = JSONReference
  { uri :: Text
  , pointer :: JSONPointer
  }
  deriving (Eq)
instance Show JSONReference where
  show ref = unpack ref.uri ++ show ref.pointer


jsonReference :: Text -> JSONReference
jsonReference t =
  let (uri, rest) = T.breakOn "#" t
   in JSONReference uri (jsonPointer rest)


jsonPointer :: Text -> JSONPointer
jsonPointer t =
  let segs = filter (not . T.null) $ T.splitOn "/" $ T.dropWhile (== '#') t
   in JSONPointer $ Path (fmap ref segs)
 where
  ref :: Text -> Ref
  ref s = fromMaybe (Child s) $ do
    n <- readMaybe (unpack s)
    pure $ Index n


newtype JSONPointer = JSONPointer Path
  deriving (Eq)
instance Show JSONPointer where
  show (JSONPointer ps) = "#/" ++ show ps


newtype Anchor = Anchor {anchor :: Text}
  deriving (Show, Eq)
  deriving newtype (IsString)


newtype Anchors = Anchors [(Anchor, Value)]
  deriving (Show, Eq)
  deriving newtype (Monoid, Semigroup)

-- data AsdfDocument = AsdfDocument
--   { tree :: Tree
--   , anchors :: Anchors
--   }

--
-- pure $ Reference uri _

-- schemaTag :: forall a. (Schema a, KnownSymbol (Tag a)) => SchemaTag
-- schemaTag = SchemaTag $ pack $ symbolVal @(Tag a) Proxy

-- matchSchemaTag :: forall a. (Schema a, KnownSymbol (Tag a)) => Maybe SchemaTag -> Parser ()
-- matchSchemaTag Nothing =
--   fail $ "Missing Schema Tag. Expected " <> show (schemaTag @a)
-- matchSchemaTag (Just s) = do
--   let sexp = schemaTag @a
--   unless (s == sexp) $ do
--     fail $ "Mismatched Schema Tag." ++ expected (show sexp) s

-- NDArray -------------------------------------------
