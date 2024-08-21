{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Node where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.Int
import System.ByteOrder (ByteOrder (..))
import Telescope.Data.Axes


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
  deriving (Eq)


instance Show NDArrayData where
  show nd = unwords ["NDArrayData", show (BS.length nd.bytes), show nd.byteorder, show nd.shape]


data DataType
  = Float64
  | Int64
  | Int32
  | Int16
  | Int8
  deriving (Show, Eq)


class IsDataType a where
  dataType :: DataType


instance IsDataType Double where
  dataType = Float64
instance IsDataType Int64 where
  dataType = Int64
instance IsDataType Int32 where
  dataType = Int32
instance IsDataType Int16 where
  dataType = Int16
instance IsDataType Int8 where
  dataType = Int8
instance (IsDataType a) => IsDataType [a] where
  dataType = dataType @a

-- ndArray :: (ToNDArray a) => a -> Value
-- ndArray a = NDArray $ toNDArray a
