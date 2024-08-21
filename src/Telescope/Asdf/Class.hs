{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Class where

import Data.Massiv.Array (Array, Prim)
import Data.Massiv.Array qualified as M
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text, pack)
import GHC.Generics
import GHC.Int
import System.ByteOrder (ByteOrder (..))
import Telescope.Asdf.Error (expected)
import Telescope.Asdf.File (BlockSource (..))
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary


class ToAsdf a where
  toValue :: a -> Value
  default toValue :: (Generic a, GObject (Rep a)) => a -> Value
  toValue a = Object $ gToObject (from a)


  schema :: SchemaTag
  default schema :: SchemaTag
  schema = mempty


class FromAsdf a where
  parseValue :: Value -> Parser a
  default parseValue :: (Generic a, GObject (Rep a)) => Value -> Parser a
  parseValue (Object o) = to <$> gParseObject o
  parseValue val = fail $ expected "Object" val


instance ToAsdf Int where
  toValue n = toValue (fromIntegral @Int @Int64 n)
instance FromAsdf Int where
  parseValue = fmap (fromIntegral @Int64 @Int) <$> parseValue


instance ToAsdf Int32 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int32 where
  parseValue = \case
    Integer n -> pure $ fromIntegral n
    node -> fail $ expected "Int32" node


instance ToAsdf Int64 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int64 where
  parseValue = \case
    Integer n -> pure $ fromIntegral n
    node -> fail $ expected "Int64" node


instance ToAsdf Double where
  toValue n = Number $ fromFloatDigits n
instance FromAsdf Double where
  parseValue = \case
    Number n -> pure $ toRealFloat n
    node -> fail $ expected "Double" node


-- Encode 1d lists inline if less than 20 values, otherwise use ndarray
-- instance (ToAsdf a, BinaryValue a, IsDataType a) => ToAsdf [a] where
--   toValue ns =
--     if length ns < 20
--       then Array $ fmap toNode ns
--       else NDArray $ toNDArray ns
-- instance (FromAsdf a, FromNDArray a, BinaryValue a) => FromAsdf [a] where
--   parseValue = \case
--     Array ns -> mapM fromNode ns
--     NDArray a -> fromNDArray a
--     node -> fail $ expected "[Int64]" node

-- it has no way to disambiguate these. The type isn't even more specific!
-- this is the generic implementation
-- but we might want to have specifics!

-- NOTE: This is the "correct" instance.
instance (FromAsdf a) => FromAsdf [a] where
  parseValue = \case
    Array ns -> mapM (parseNode @a) ns
    node -> fail $ expected "Array" node
instance (ToAsdf a) => ToAsdf [a] where
  toValue as = Array $ fmap toNode as


instance (FromAsdf a) => FromAsdf (Maybe a) where
  parseValue = \case
    Null -> pure Nothing
    val -> Just <$> parseValue @a val
instance (ToAsdf a) => ToAsdf (Maybe a) where
  toValue Nothing = Null
  toValue (Just a) = toValue a


instance (BinaryValue a, Prim a, AxesIndex ix) => FromAsdf (Array M.D ix a) where
  parseValue = \case
    NDArray a -> fromNDArray a
    node -> fail $ expected "Array" node
instance (BinaryValue a, IsDataType a, Prim a, AxesIndex ix, PutArray ix) => ToAsdf (Array M.D ix a) where
  toValue as = NDArray $ ndArrayMassiv as


instance ToAsdf Text where
  toValue = String
instance FromAsdf Text where
  parseValue = \case
    String t -> pure t
    node -> fail $ expected "Text" node


instance ToAsdf Value where
  toValue = id
instance FromAsdf Value where
  parseValue = pure


instance ToAsdf NDArrayData where
  toValue = NDArray
instance FromAsdf NDArrayData where
  parseValue = \case
    NDArray nda -> pure nda
    node -> fail $ expected "NDArray" node


instance ToAsdf DataType where
  toValue Float64 = "float64"
  toValue Int64 = "int64"
  toValue Int32 = "int32"
  toValue Int16 = "int16"
  toValue Int8 = "int8"
instance FromAsdf DataType where
  parseValue = \case
    String "float64" -> pure Float64
    String "int64" -> pure Int64
    String "int32" -> pure Int32
    String "int16" -> pure Int16
    String "int8" -> pure Int8
    val -> fail $ expected "DataType" val


instance ToAsdf ByteOrder where
  toValue = \case
    BigEndian -> "big"
    LittleEndian -> "little"
instance FromAsdf ByteOrder where
  parseValue = \case
    String "big" -> pure BigEndian
    String "little" -> pure LittleEndian
    node -> fail $ expected "ByteOrder" node


instance ToAsdf (Axes Row) where
  toValue (Axes as) = toValue as


instance ToAsdf BlockSource where
  toValue (BlockSource s) = toValue s


-- | Convert to a Node, including the schema tag if specified
toNode :: forall a. (ToAsdf a) => a -> Node
toNode a = Node (schema @a) $ toValue a


-- | Parse a node, ignoring the schema tag
parseNode :: (FromAsdf a) => Node -> Parser a
parseNode (Node _ v) = parseValue v


(.:) :: (FromAsdf a) => Object -> Key -> Parser a
o .: k = addContext (Child k) $ do
  node <- parseKey k o
  parseNode node


(.:?) :: (FromAsdf a) => Object -> Key -> Parser (Maybe a)
o .:? k = addContext (Child k) $ do
  case lookup k o of
    Nothing -> pure Nothing
    Just a -> Just <$> parseNode a


-- data Scalar
--   = SInt8
--   | SInt16
--   | SInt32
--   | SInt64
--   | SUInt8
--   | SUInt16
--   | SUInt32
--   | SUInt64
--   | SFloat16
--   | SFloat32
--   | SFloat64
--   | SComplex64
--   | SComplex128
--   | SBool8
--   | SAscii Length
--   | SUCS4 Length

-- type Length = Int
--
--
-- data DataType
--   = Scalar Scalar

class GObject f where
  gToObject :: f p -> Object
  gParseObject :: Object -> Parser (f p)


instance (GObject f) => GObject (M1 D c f) where
  gToObject (M1 f) = gToObject f
  gParseObject o = M1 <$> gParseObject o


instance (GObject f) => GObject (M1 C c f) where
  gToObject (M1 f) = gToObject f
  gParseObject o = M1 <$> gParseObject o


instance (GObject f, GObject g) => GObject (f :*: g) where
  gToObject (f :*: g) = gToObject f <> gToObject g
  gParseObject o = do
    f <- gParseObject o
    g <- gParseObject o
    pure $ f :*: g


instance (GNode f, Selector s) => GObject (M1 S s f) where
  gToObject (M1 f) =
    let s = selName (undefined :: M1 S s f p)
     in [(pack s, gToNode f)]
  gParseObject o = do
    let k = pack $ selName (undefined :: M1 S s f p)
    M1 <$> gParseKey o k


class GNode f where
  gToNode :: f p -> Node
  gParseKey :: Object -> Key -> Parser (f p)


instance {-# OVERLAPPABLE #-} (ToAsdf a, FromAsdf a) => GNode (K1 R a) where
  gToNode (K1 a) = toNode a
  gParseKey o k = K1 <$> o .: k


instance {-# OVERLAPPING #-} (ToAsdf a, FromAsdf a) => GNode (K1 R (Maybe a)) where
  gToNode (K1 a) = toNode a
  gParseKey o k = K1 <$> o .:? k
