module Telescope.Asdf.Node where

import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty)
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
  , value :: Value
  }
  deriving (Eq)
instance Show Node where
  show (Node st v) = show st ++ show v
instance IsString Node where
  fromString s = Node mempty $ String $ pack s


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
  | ExternalRef !Reference
  | InternalRef !Pointer
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
fromValue = Node mempty


newtype Tree = Tree Object
  deriving newtype (Semigroup, Monoid)


-- always $ref: uri#path
data Reference = Reference
  { uri :: Text
  , pointer :: Pointer
  }
  deriving (Show, Eq)


reference :: Text -> Maybe Reference
reference t = do
  [uri, rest] <- pure $ T.split (== '#') t
  guard (not $ T.null uri)
  let point = pointer rest
  pure $ Reference uri point


pointer :: Text -> Pointer
pointer t =
  let segs = filter (not . T.null) $ T.splitOn "/" $ T.dropWhile (== '#') t
   in Pointer $ Path (fmap ref segs)
 where
  ref :: Text -> Ref
  ref s = fromMaybe (Child s) $ do
    n <- readMaybe (unpack s)
    pure $ Index n


newtype Pointer = Pointer Path
  deriving (Show, Eq)

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
