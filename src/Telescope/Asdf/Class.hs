{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf.Class where

import Control.Monad (replicateM, unless)
import Control.Monad.Catch (MonadThrow)
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array (Array, D, Index, Ix1, Ix2, Ix3, Ix4, Ix5, Manifest, MonadUnliftIO, Prim)
import Data.Massiv.Array qualified as M
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import GHC.Int
import GHC.TypeLits
import System.ByteOrder (ByteOrder (..), Bytes (..))
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Fits.Types (Axes (..), Row)


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


-- Encode 1d lists inline if less than 20 values, otherwise use ndarray
instance (BinaryValue a, ToAsdf a, IsDataType a) => ToAsdf [a] where
  toValue ns =
    if length ns < 20
      then Array $ fmap toNode ns
      else NDArray $ toNDArray ns
instance (BinaryValue a, FromAsdf a) => FromAsdf [a] where
  parseValue = \case
    Array ns -> mapM fromNode ns
    NDArray a -> fromNDArray a
    node -> fail $ expected "[Int64]" node


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




(.:) :: (FromAsdf a) => Object -> Key -> Parser a
o .: k = do
  node <- parseNode k o
  fromNode node

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
