{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Node where

import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, D, Ix1, Ix2, Ix3, Ix4, Ix5)
import Data.Massiv.Array qualified as M
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.Int
import System.ByteOrder (ByteOrder (..), Bytes (..))
import Telescope.Fits.Types (Axes (..), Row, axesRowMajor)

import Control.Monad (replicateM, unless)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


class ToAsdf a where
  toValue :: a -> Value
  schema :: SchemaTag
  default schema :: SchemaTag
  schema = mempty


class FromAsdf a where
  parseValue :: Value -> Parser a


instance ToAsdf ByteOrder where
  toValue = \case
    BigEndian -> "big"
    LittleEndian -> "little"
instance FromAsdf ByteOrder where
  parseValue = \case
    String "big" -> pure BigEndian
    String "little" -> pure LittleEndian
    node -> fail $ expected "ByteOrder" node


instance ToAsdf Int where
  toValue n = toValue (fromIntegral @Int @Int64 n)
instance FromAsdf Int where
  parseValue = fmap (fromIntegral @Int64 @Int) <$> parseValue


instance ToAsdf Int64 where
  toValue n = Number $ fromIntegral n
instance FromAsdf Int64 where
  parseValue = \case
    Number n -> pure $ round n
    node -> fail $ expected "Int64" node


instance ToAsdf Double where
  toValue n = Number $ fromFloatDigits n
instance FromAsdf Double where
  parseValue = \case
    Number n -> pure $ toRealFloat n
    node -> fail $ expected "Double" node


-- encode differently based on the length
instance ToAsdf [Int64] where
  toValue ns =
    if length ns < 20
      then Array $ fmap toNode ns
      else ndArray ns Int64 (axesRowMajor [length ns])
instance FromAsdf [Int64] where
  parseValue = \case
    Array ns -> mapM fromNode ns
    NDArray a -> fromNDArray a
    node -> fail $ expected "[Int64]" node


-- Only short arrays (10 or less) are serialized inline
instance ToAsdf [Double] where
  toValue ns =
    if length ns < 10
      then Array $ fmap toNode ns
      else ndArray ns Float64 (axesRowMajor [length ns])
instance FromAsdf [Double] where
  parseValue = \case
    Array ns -> mapM fromNode ns
    NDArray a -> fromNDArray a
    node -> fail $ expected "[Double]" node


instance ToAsdf Text where
  toValue = String
instance FromAsdf Text where
  parseValue = \case
    String t -> pure t
    node -> fail $ expected "Text" node


-- instance ToAsdf ByteString where
--   toValue = Binary
-- instance FromAsdf ByteString where
--   parseValue = \case
--     Binary t -> pure t
--     node -> fail $ expected "Binary" node

instance ToAsdf (Axes Row) where
  toValue (Axes axs) = Array $ fmap axis axs
   where
    axis ax = Node mempty (Number $ fromIntegral ax)
instance FromAsdf (Axes Row) where
  parseValue = \case
    Array ns -> do
      axes <- mapM (\(Node _ v) -> parseValue v) ns
      pure $ Axes axes
    node -> fail $ expected "Axes" node


instance ToAsdf Value where
  toValue = id
instance FromAsdf Value where
  parseValue = pure


-- | Convert to a Node, including the schema tag if specified
toNode :: forall a. (ToAsdf a) => a -> Node
toNode a = Node (schema @a) $ toValue a


-- | Parse a node, ignoring the schema tag
fromNode :: (FromAsdf a) => Node -> Parser a
fromNode (Node _ v) = parseValue v


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
  | -- | RawBinary !ByteString
    NDArray !NDArrayData
  | Array ![Node]
  | Object !Object
  | Null
  deriving (Show, Eq)
instance IsString Value where
  fromString = String . pack


type Key = Text
type Object = [(Key, Node)]


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

-- NDArray -------------------------------------------

{- | In-tree representation of an NDArray. You can parse a file as this and get it back. Not really what we want though
but in haskell we can't easily just parse a multi-dimensional array
we could do a simpler representation. Using an ADT
-}

-- TODO: byteswap before constructing while parsing if byteorder = "little"
--  * NOPE You can't byteswap without knowing the datatype
--  * NOPE You can't parse the bytestring without knowing the datatype
--
--  so we have to pass the byteorder on to the user?
data NDArrayData = NDArrayData
  { bytes :: ByteString
  , byteorder :: ByteOrder
  , datatype :: DataType
  , shape :: Axes Row
  }
  deriving (Show, Eq)


data DataType
  = Float64
  | Int64
  deriving (Show, Eq)


fromNDArray :: (BinaryValue a) => NDArrayData -> Parser a
fromNDArray dat =
  decode dat.byteorder dat.bytes
 where
  decode bo bytes =
    case runGetOrFail (get bo) (BL.fromStrict bytes) of
      -- TODO: better error message, including source num? Not sure if it's possible if we do it this way
      Left (rest, nused, err) ->
        fail $ "could not decode binary data at (" ++ show nused ++ ") (rest " ++ show (BL.length rest) ++ "): " ++ err
      Right (_, _, a) -> pure a


-- could be an instance instead... any [Double] for example could serialize this way
parseNDArray :: (BinaryValue a) => Value -> Parser a
parseNDArray val = do
  dat <- ndarray val
  fromNDArray dat
 where
  ndarray (NDArray a) = pure a
  ndarray v = fail $ expected "NDArray" v


ndArray :: (BinaryValue a) => a -> DataType -> Axes Row -> Value
ndArray a datatype shape =
  let bytes = BL.toStrict $ runPut (put byteorder a)
      -- we always serialize to BigEndian
      byteorder = BigEndian
   in NDArray $ NDArrayData{bytes, byteorder, datatype, shape}


class BinaryValue a where
  put :: ByteOrder -> a -> Put
  get :: ByteOrder -> Get a


instance BinaryValue Int64 where
  put BigEndian = putInt64be
  put LittleEndian = putInt64le
  get BigEndian = getInt64be
  get LittleEndian = getInt64le


instance BinaryValue Double where
  put BigEndian = putDoublebe
  put LittleEndian = putDoublele
  get BigEndian = getDoublebe
  get LittleEndian = getDoublele


instance (BinaryValue a) => BinaryValue [a] where
  put bo = mapM_ (put bo)
  get bo = getUntilEmpty (get bo)


getUntilEmpty :: Get a -> Get [a]
getUntilEmpty gt = do
  empty <- isEmpty
  if empty
    then return []
    else do
      a <- gt
      as <- getUntilEmpty gt
      pure (a : as)


instance (BinaryValue a) => BinaryValue (Array D Ix1 a) where
  put bo = mapM_ (put bo)
  get bo = do
    let v = parseVector @a Par f inp
    let num = 10
    ns <- replicateM num (get bo)
    pure $ M.fromList M.Seq ns

-- 1. [Int64] - keep parsing ints until the end of the list
-- 2. [Double] - same
-- 4. Array D ix a - shape is known at all times
--
-- XXX. exploration: parse everything as a Value so you can check out the values...
-- XXX. [[Int64]] - now we need to know the shape to decode. It could be anything! The encoding we have the shape, no problem, but not decoding

-- ok, ByteStrings are just Word8s
-- but that's not really what I want, is it?

-- can we parse it into machine-native endianness?
-- byte-swap it into Big endian / binary
-- data BinaryData = BinaryData
--   { bytes :: ByteString -- ByteString ~ [Word8]
--   , byteorder :: ByteOrder
--   , datatype :: DataType
--   , shape :: Axes Row
--   }
--   deriving (Show, Eq)
--
--
-- NOTE: This spec is human-readable. There's no reason to parse it dynamically
-- data UnknownArray a
--   = UA1 (Array D Ix1 a)
--   | UA2 (Array D Ix2 a)
--   | UA3 (Array D Ix3 a)
--
--
--   what about with a `Little` newtype?
--   then we could use Binary instances...
--
--
-- WARNING: We *can't* use the Binary instance of [a]. It encodes it as [LENGTH][0][1][2][...]
--
--
