{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Node where

import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.ByteOrder (ByteOrder (..))
import Telescope.Fits.Types (Axes (..), Row)

import Control.Monad (unless)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


class ToAsdf a where
  toValue :: a -> Value
  schema :: SchemaTag
  default schema :: SchemaTag
  schema = mempty


class FromAsdf a where
  fromValue :: Value -> Parser a


instance ToAsdf ByteOrder where
  toValue = \case
    BigEndian -> "big"
    LittleEndian -> "little"
instance FromAsdf ByteOrder where
  fromValue = \case
    String "big" -> pure BigEndian
    String "little" -> pure LittleEndian
    node -> fail $ expected "ByteOrder" node


instance ToAsdf Int where
  toValue n = Number $ fromIntegral n
instance FromAsdf Int where
  fromValue = \case
    Number n -> pure $ round n
    node -> fail $ expected "Int" node


instance ToAsdf Text where
  toValue = String
instance FromAsdf Text where
  fromValue = \case
    String t -> pure t
    node -> fail $ expected "Text" node


instance ToAsdf ByteString where
  toValue = Binary
instance FromAsdf ByteString where
  fromValue = \case
    Binary t -> pure t
    node -> fail $ expected "Binary" node


instance ToAsdf (Axes Row) where
  toValue (Axes axs) = Array $ fmap axis axs
   where
    axis ax = Node mempty (Number $ fromIntegral ax)
instance FromAsdf (Axes Row) where
  fromValue = \case
    Array ns -> do
      axes <- mapM (fromValue . (.value)) ns
      pure $ Axes axes
    node -> fail $ expected "Axes" node


instance ToAsdf Value where
  toValue = id
instance FromAsdf Value where
  fromValue = pure


-- | Convert to a Node using a schema if specified
toNode :: forall a. (ToAsdf a) => a -> Node
toNode a = Node (schema @a) $ toValue a


-- | Parse a node. We don't enforce the schema matching
fromNode :: (FromAsdf a) => Node -> Parser a
fromNode (Node _ v) = fromValue v


-- | Specify a schema using 'schema' from 'ToAsdf'
newtype SchemaTag = SchemaTag (Maybe Text)
  deriving (Show)
  deriving newtype (Monoid, Semigroup, Eq)


instance IsString SchemaTag where
  fromString s = SchemaTag (Just $ pack s)


data Node = Node
  { schema :: SchemaTag
  , value :: Value
  }
  deriving (Show, Eq)


-- We can't use Aeson's Value, because it doesn't support tags or binary data
data Value
  = Bool !Bool
  | Number !Scientific
  | String !Text
  | Binary !ByteString -- replaces with integer source location
  | Array ![Node]
  | Object !Object
  | Null
  deriving (Show, Eq)
instance IsString Value where
  fromString = String . pack


type Object = [(Text, Node)]
type Key = Text


newtype Document = Document Node


(.:) :: (FromAsdf a) => Object -> Key -> Parser a
o .: k = do
  node <- parseNode k o
  fromNode node


parseNode :: Key -> Object -> Parser Node
parseNode k o =
  case lookup k o of
    Nothing -> fail $ "Missing key: " ++ show k
    Just node -> pure node


expected :: (Show a) => String -> a -> String
expected ex n = "Expected " ++ ex ++ ", but got: " ++ show n

-- schemaTag :: forall a. (Schema a, KnownSymbol (Tag a)) => SchemaTag
-- schemaTag = SchemaTag $ pack $ symbolVal @(Tag a) Proxy

-- matchSchemaTag :: forall a. (Schema a, KnownSymbol (Tag a)) => Maybe SchemaTag -> Parser ()
-- matchSchemaTag Nothing =
--   fail $ "Missing Schema Tag. Expected " <> show (schemaTag @a)
-- matchSchemaTag (Just s) = do
--   let sexp = schemaTag @a
--   unless (s == sexp) $ do
--     fail $ "Mismatched Schema Tag." ++ expected (show sexp) s
