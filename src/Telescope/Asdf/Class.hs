{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Class where

import Data.List ((!?))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, Prim)
import Data.Massiv.Array qualified as M
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import Effectful
import Effectful.Fail
import GHC.Generics
import GHC.Int
import Telescope.Asdf.Encoding.File (BlockSource (..))
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Data.Parser


{- | Convert a type to an Asdf 'Value' or 'Node'. The generic instance will encode to an 'Object' with field names matching record selectors

> data Example = Example
>   { name :: Text
>   , age :: Int
>   , tags :: [Text]
>   }
>   deriving (Generic, ToAsdf)
>
> instance ToAsdf Example where
>   schema _ = "tag:example.org/schemas/example-1.0.0"
-}
class ToAsdf a where
  -- | Specify how an object encodes to a 'Value'
  --
  -- > instance User ToAsdf where
  -- >   toValue user =
  -- >     Object
  -- >       [ ("name", toNode user.name)
  -- >       , ("age", toNode user.age)
  -- >       ]
  toValue :: a -> Value
  default toValue :: (Generic a, GToObject (Rep a)) => a -> Value
  toValue a = Object $ gToObject (from a)


  -- | Specify the schema for a type
  --
  -- > instance ToAsdf Unit where
  -- >   schema _ = "!unit/unit-1.0.0"
  schema :: a -> SchemaTag
  default schema :: a -> SchemaTag
  schema _ = mempty


  -- | Specify that this node be saved as an anchor
  --
  -- > instance ToAsdf Config where
  -- >   anchor _ = Just "globalConfig"
  anchor :: a -> Maybe Anchor
  default anchor :: a -> Maybe Anchor
  anchor _ = Nothing


  -- | Manually control all aspects of how this is converted to a 'Node'
  toNode :: a -> Node
  default toNode :: a -> Node
  toNode a = Node (schema a) (anchor a) $ toValue a


{- | Parse an Asdf 'Value' or 'Node' into a type. The generic instance will decode an 'Object' with field names matching record selectors

> data Example = Example
>   { name :: Text
>   , age :: Int
>   , tags :: [Text]
>   }
>   deriving (Generic, FromAsdf)
-}
class FromAsdf a where
  -- | Specify how a type is parsed from a 'Value'
  --
  -- > instance FromAsdf Integer where
  -- >   parseValue = \case
  -- >     Integer n -> pure $ fromIntegral n
  -- >     node -> expected "Integer" node
  parseValue :: (Parser :> es) => Value -> Eff es a
  default parseValue :: (Generic a, GParseObject (Rep a), Parser :> es) => Value -> Eff es a
  parseValue (Object o) = to <$> gParseObject o
  parseValue val = expected "Object" val


instance ToAsdf Int where
  toValue n = toValue (fromIntegral @Int @Int64 n)
instance FromAsdf Int where
  parseValue = fmap (fromIntegral @Int64 @Int) <$> parseValue


instance ToAsdf Int8 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int8 where
  parseValue = parseInteger


instance ToAsdf Int16 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int16 where
  parseValue = parseInteger


instance ToAsdf Int32 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int32 where
  parseValue = parseInteger


instance ToAsdf Int64 where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Int64 where
  parseValue = parseInteger


instance ToAsdf Integer where
  toValue n = Integer $ fromIntegral n
instance FromAsdf Integer where
  parseValue = parseInteger


instance ToAsdf Double where
  toValue n = Number $ fromFloatDigits n
instance FromAsdf Double where
  parseValue = \case
    Number n -> pure $ toRealFloat n
    node -> expected "Double" node


instance ToAsdf Float where
  toValue n = Number $ fromFloatDigits n
instance FromAsdf Float where
  parseValue = \case
    Number n -> pure $ toRealFloat n
    node -> expected "Float" node


parseInteger :: (Integral a, Parser :> es) => Value -> Eff es a
parseInteger = \case
  Integer n -> pure $ fromIntegral n
  node -> expected "Integer" node


instance {-# OVERLAPPABLE #-} (FromAsdf a) => FromAsdf [a] where
  parseValue = \case
    Array ns -> mapM (parseNode @a) ns
    node -> expected "Array" node
instance {-# OVERLAPPABLE #-} (ToAsdf a) => ToAsdf [a] where
  toValue as = Array $ fmap toNode as
instance {-# OVERLAPPABLE #-} (FromAsdf a) => FromAsdf (NonEmpty a) where
  parseValue val = do
    as <- parseValue @[a] val
    case as of
      [] -> expected "NonEmpty List" val
      (a : rest) -> pure (a :| rest)
instance {-# OVERLAPPABLE #-} (ToAsdf a) => ToAsdf (NonEmpty a) where
  toValue as = toValue $ NE.toList as


instance (ToAsdf a, ToAsdf b) => ToAsdf (a, b) where
  toValue (a, b) = Array [toNode a, toNode b]
instance (FromAsdf a, FromAsdf b) => FromAsdf (a, b) where
  parseValue = \case
    Array [n1, n2] -> do
      a <- parseValue n1.value
      b <- parseValue n2.value
      pure (a, b)
    node -> expected "[a, b]" node


instance (ToAsdf a, ToAsdf b, ToAsdf c) => ToAsdf (a, b, c) where
  toValue (a, b, c) = Array [toNode a, toNode b, toNode c]
instance (FromAsdf a, FromAsdf b, FromAsdf c) => FromAsdf (a, b, c) where
  parseValue = \case
    Array [na, nb, nc] -> do
      a <- parseValue na.value
      b <- parseValue nb.value
      c <- parseValue nc.value
      pure (a, b, c)
    node -> expected "[a, b, c]" node


instance (ToAsdf a, ToAsdf b, ToAsdf c, ToAsdf d) => ToAsdf (a, b, c, d) where
  toValue (a, b, c, d) = Array [toNode a, toNode b, toNode c, toNode d]
instance (FromAsdf a, FromAsdf b, FromAsdf c, FromAsdf d) => FromAsdf (a, b, c, d) where
  parseValue = \case
    Array [na, nb, nc, nd] -> do
      a <- parseValue na.value
      b <- parseValue nb.value
      c <- parseValue nc.value
      d <- parseValue nd.value
      pure (a, b, c, d)
    node -> expected "[a, b, c, d]" node


-- they will always serialize to Array
instance FromAsdf [Text] where
  parseValue = parseAnyList
instance FromAsdf [Int] where
  parseValue = parseAnyList
instance FromAsdf [Int8] where
  parseValue = parseAnyList
instance FromAsdf [Int16] where
  parseValue = parseAnyList
instance FromAsdf [Int32] where
  parseValue = parseAnyList
instance FromAsdf [Int64] where
  parseValue = parseAnyList
instance FromAsdf [Double] where
  parseValue = parseAnyList


-- | Flexibly parse lists from either Array or NDArray
parseAnyList :: (FromAsdf a, FromNDArray [a], Parser :> es) => Value -> Eff es [a]
parseAnyList = \case
  Array ns -> mapM parseNode ns
  NDArray dat -> fromNDArray dat
  node -> expected "[Double]" node


instance (FromAsdf a) => FromAsdf (Maybe a) where
  parseValue = \case
    Null -> pure Nothing
    val -> Just <$> parseValue @a val
instance (ToAsdf a) => ToAsdf (Maybe a) where
  schema = maybe mempty schema
  anchor = maybe Nothing anchor
  toValue Nothing = Null
  toValue (Just a) = toValue a


instance (BinaryValue a, Prim a, AxesIndex ix) => FromAsdf (Array M.D ix a) where
  parseValue = \case
    NDArray a -> fromNDArray a
    node -> expected "NDArray" node
instance (BinaryValue a, IsDataType a, Prim a, AxesIndex ix, PutArray ix) => ToAsdf (Array M.D ix a) where
  toValue as = NDArray $ ndArrayMassiv as


instance ToAsdf Text where
  toValue = String
instance FromAsdf Text where
  parseValue = \case
    String t -> pure t
    node -> expected "Text" node


instance ToAsdf String where
  toValue = String . pack
instance FromAsdf String where
  parseValue = \case
    String t -> pure $ unpack t
    node -> expected "Text" node


instance ToAsdf Bool where
  toValue = Bool
instance FromAsdf Bool where
  parseValue = \case
    Bool b -> pure b
    node -> expected "Bool" node


instance ToAsdf Value where
  toValue = id
instance FromAsdf Value where
  parseValue = pure


instance ToAsdf Node where
  toValue (Node _ _ val) = val
instance FromAsdf Node where
  parseValue val = pure $ Node mempty Nothing val


instance ToAsdf Tree where
  toValue (Tree o) = Object o
instance FromAsdf Tree where
  parseValue = \case
    Object o -> pure $ Tree o
    val -> expected "Object" val


instance ToAsdf NDArrayData where
  toValue = NDArray
instance FromAsdf NDArrayData where
  parseValue = \case
    NDArray nda -> pure nda
    node -> expected "NDArray" node


instance ToAsdf DataType where
  toValue Float64 = "float64"
  toValue Float32 = "float32"
  toValue Int64 = "int64"
  toValue Int32 = "int32"
  toValue Int16 = "int16"
  toValue Int8 = "int8"
  toValue Bool8 = "bool8"
  toValue (Ucs4 n) = Array ["ucs4", fromValue $ Integer $ fromIntegral n]
instance FromAsdf DataType where
  parseValue = \case
    String "float64" -> pure Float64
    String "float32" -> pure Float32
    String "int64" -> pure Int64
    String "int32" -> pure Int32
    String "int16" -> pure Int16
    String "int8" -> pure Int8
    String "bool8" -> pure Bool8
    Array ["ucs4", Node _ _ (Integer n)] -> pure $ Ucs4 $ fromIntegral n
    val -> expected "DataType" val


instance ToAsdf ByteOrder where
  toValue = \case
    BigEndian -> "big"
    LittleEndian -> "little"
instance FromAsdf ByteOrder where
  parseValue = \case
    String "big" -> pure BigEndian
    String "little" -> pure LittleEndian
    node -> expected "ByteOrder" node


instance ToAsdf (Axes Row) where
  toValue (Axes as) = toValue as


instance ToAsdf BlockSource where
  toValue (BlockSource s) = toValue s


instance ToAsdf UTCTime where
  toValue t = String $ pack $ iso8601Show t
instance FromAsdf UTCTime where
  parseValue v = do
    ts <- parseValue @String v
    res <- runFail $ iso8601ParseM ts
    case res of
      Left e -> parseFail e
      Right a -> pure a


-- | Parse a node, ignoring the schema tag
parseNode :: (FromAsdf a, Parser :> es) => Node -> Eff es a
parseNode (Node _ _ v) = parseValue v


{- | Parse a key from an 'Object'

> instance FromAsdf User where
>   parseValue = \case
>     Object o -> do
>       name <- o .: "name"
>       age <- o .: "age"
>       pure $ User{name, age}
-}
(.:) :: (FromAsdf a, Parser :> es) => Object -> Key -> Eff es a
o .: k = do
  case lookup k o of
    Nothing -> parseFail $ "key " ++ show k ++ " not found"
    Just node ->
      parseAt (Child k) $ parseNode node


-- | Parse an optional key from an 'Object'
(.:?) :: (FromAsdf a, Parser :> es) => Object -> Key -> Eff es (Maybe a)
o .:? k = do
  case lookup k o of
    Nothing -> pure Nothing
    Just a ->
      Just <$> do
        parseAt (Child k) $ parseNode a


{- | Parse a child at the given array index
 -
> instance FromAsdf Friends where
>   parseValue = \case
>     Array ns -> do
>       best <- ns ! 0
>       second <- ns ! 1
>       other <- mapM parseNode ns
>       pure $ Friends{best, second, other}
-}
(!) :: (FromAsdf a, Parser :> es) => [Node] -> Int -> Eff es a
ns ! n = do
  case ns !? n of
    Nothing -> parseFail $ "Index " ++ show n ++ " not found"
    Just node ->
      parseAt (Index n) $ parseNode node


-- | Generically serialize records to an 'Object'
class GToObject f where
  gToObject :: f p -> Object


instance (GToObject f) => GToObject (M1 D c f) where
  gToObject (M1 f) = gToObject f


instance (GToObject f) => GToObject (M1 C c f) where
  gToObject (M1 f) = gToObject f


instance (GToObject f, GToObject g) => GToObject (f :*: g) where
  gToObject (f :*: g) = gToObject f <> gToObject g


instance (GToNode f, Selector s) => GToObject (M1 S s f) where
  gToObject (M1 f) =
    let s = selName (undefined :: M1 S s f p)
     in [(pack s, gToNode f)]


-- | Generically serialize record values to a 'Node'
class GToNode f where
  gToNode :: f p -> Node


instance {-# OVERLAPPABLE #-} (ToAsdf a) => GToNode (K1 R a) where
  gToNode (K1 a) = toNode a


instance {-# OVERLAPPING #-} (ToAsdf a) => GToNode (K1 R (Maybe a)) where
  gToNode (K1 a) = toNode a


-- | Generically parse 'Object's into records
class GParseObject f where
  gParseObject :: (Parser :> es) => Object -> Eff es (f p)


instance (GParseObject f) => GParseObject (M1 D c f) where
  gParseObject o = M1 <$> gParseObject o


instance (GParseObject f) => GParseObject (M1 C c f) where
  gParseObject o = M1 <$> gParseObject o


instance (GParseObject f, GParseObject g) => GParseObject (f :*: g) where
  gParseObject o = do
    f <- gParseObject o
    g <- gParseObject o
    pure $ f :*: g


instance (GParseKey f, Selector s) => GParseObject (M1 S s f) where
  gParseObject o = do
    let k = pack $ selName (undefined :: M1 S s f p)
    M1 <$> gParseKey o k


-- | Generically parse a key from an 'Object' into a record value
class GParseKey f where
  gParseKey :: (Parser :> es) => Object -> Key -> Eff es (f p)


instance {-# OVERLAPPABLE #-} (FromAsdf a) => GParseKey (K1 R a) where
  gParseKey o k = K1 <$> o .: k


instance {-# OVERLAPPABLE #-} (FromAsdf a) => GParseKey (K1 R (Maybe a)) where
  gParseKey o k = K1 <$> o .:? k
