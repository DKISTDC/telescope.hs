{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Node where

import Control.Monad.Catch (MonadThrow)
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, D, Index, Ix1, Ix2, Ix3, Ix4, Ix5, Manifest, MonadUnliftIO, Prim)
import Data.Massiv.Array qualified as M
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.Int
import System.ByteOrder (ByteOrder (..), Bytes (..))
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Fits.Types (Axes (..), Row)

import Control.Monad (replicateM, unless)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


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


class IsDataType a where
  dataType :: DataType


instance IsDataType Double where
  dataType = Float64
instance IsDataType Int64 where
  dataType = Int64
instance (IsDataType a) => IsDataType [a] where
  dataType = dataType @a

-- ndArray :: (ToNDArray a) => a -> Value
-- ndArray a = NDArray $ toNDArray a
