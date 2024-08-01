{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Node where

import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.ByteOrder (ByteOrder (..))
import Telescope.Fits.Types (Axes, Row)

import Control.Monad (unless)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


class ToAsdf a where
  type Schema a :: Symbol


  toNode :: a -> Node
  default toNode :: (KnownSymbol (Schema a)) => a -> Node
  toNode a = Node (Just (schemaTag @a)) $ toValue a


  fromNode :: Node -> Parser a
  default fromNode :: (KnownSymbol (Schema a)) => Node -> Parser a
  fromNode (Node ms v) = do
    matchSchemaTag @a ms
    fromValue v


  toValue :: a -> Value


  fromValue :: Value -> Parser a


newtype SchemaTag = SchemaTag Text
  deriving (Show, Eq)


data Node = Node
  { schema :: Maybe SchemaTag
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


type Object = [(Text, Node)]
type Key = Text


instance IsString Value where
  fromString = String . pack


newtype Document = Document Node


schemaTag :: forall a. (ToAsdf a, KnownSymbol (Schema a)) => SchemaTag
schemaTag = SchemaTag $ pack $ symbolVal @(Schema a) Proxy


-- TODO: tag version doesn't need to match
matchSchemaTag :: forall a. (ToAsdf a, KnownSymbol (Schema a)) => Maybe SchemaTag -> Parser ()
matchSchemaTag Nothing =
  fail $ "Missing Schema Tag. Expected " <> show (schemaTag @a)
matchSchemaTag (Just s) = do
  let sexp = schemaTag @a
  unless (s == sexp) $ do
    fail $ "Mismatched Schema Tag. Expected " <> show sexp <> " but got " <> show s


(.:) :: (ToAsdf a) => Object -> Key -> Parser a
o .: k = do
  node <- field k o
  fromNode node


field :: Key -> Object -> Parser Node
field k o =
  case lookup k o of
    Nothing -> fail $ "Missing key: " ++ show k
    Just node -> pure node
