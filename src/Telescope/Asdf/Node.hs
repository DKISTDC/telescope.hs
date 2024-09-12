module Telescope.Asdf.Node where

import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Telescope.Asdf.Error
import Telescope.Asdf.NDArray
import Telescope.Asdf.Parser


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


-- We can't use Aeson's Value, because it doesn't support tags or binary data
data Value
  = Bool !Bool
  | Number !Scientific
  | Integer !Integer
  | String !Text
  | -- | RawBinary !ByteString
    NDArray !NDArrayData
  | Array ![Node]
  | Object !Object
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

parseKey :: Text -> Object -> Parser Node
parseKey k o = do
  case lookup k o of
    Nothing -> fail $ "key " ++ show k ++ " not found"
    Just node -> pure node


parseNDArray :: (FromNDArray a) => Value -> Parser a
parseNDArray val = do
  dat <- ndarray val
  fromNDArray dat
 where
  ndarray (NDArray a) = pure a
  ndarray v = fail $ expected "NDArray" v
