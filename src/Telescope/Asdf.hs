{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text, pack, unpack)
import GHC.ByteOrder (ByteOrder (..))
import GHC.TypeLits
import Telescope.Fits.Types (Axes, Row)


-- TODO: We need an AST to describe an ASDF file
--
-- ANYTHING can have a tag. They're optional
-- but practically, basic datatypes don't have one

newtype SchemaTag = SchemaTag Text
  deriving (Show, Eq)


data Node = Node
  { schema :: Maybe SchemaTag
  , value :: Value
  }
  deriving (Show, Eq)


data Value
  = Bool !Bool
  | Number !Scientific
  | String !Text
  | Binary !ByteString -- replaces with integer source location
  | Array ![Node]
  | Object ![(Text, Node)]
  | Null
  deriving (Show, Eq)


-- WARNING: probably
data NDArray = NDArray
  { source :: ByteString
  , byteorder :: ByteOrder
  , datatype :: DataType
  , shape :: Axes Row
  }


data DataType = Float64


-- wait, is this even useful?
-- no necessarily, it doesn't encoede tags at all
-- we aren't going to be encoding a bunch of structures like thids

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


schemaTag :: forall a. (ToAsdf a, KnownSymbol (Schema a)) => SchemaTag
schemaTag = SchemaTag $ pack $ symbolVal @(Schema a) Proxy


matchSchemaTag :: forall a. (ToAsdf a, KnownSymbol (Schema a)) => Maybe SchemaTag -> Parser ()
matchSchemaTag Nothing = fail $ "Missing Schema Tag. Expected " <> show (schemaTag @a)
matchSchemaTag (Just s) = do
  let sexp = schemaTag @a
  unless (s == sexp) $ do
    fail $ "Mismatched Schema Tag. Expected " <> show sexp <> " but got " <> show s

-- - !core/column-1.0.0
--   data: !core/ndarray-1.0.0
--     source: 254
--     datatype: int64
--     byteorder: little
--     shape: [340]
--   name: VSPMAP

-- EXAMPLE ------------------------------------------------------
-- frame: !<tag:stsci.edu:gwcs/frame-1.0.0>
--   axes_names: [spatial along slit, dispersion axis, raster scan step number,
--     polarization state]
--   axes_order: [0, 1, 2, 3]
--   axes_type: [PIXEL, PIXEL, PIXEL, PIXEL]
--   axis_physical_types: ['custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL']
--   name: pixel
--   naxes: 4
--   unit: [!unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel,
--     !unit/unit-1.0.0 pixel]
--
--
-- EXAMPLE -------------------------------------------------------
-- - !unit/quantity-1.1.0 {unit: !unit/unit-1.0.0 pixel, value: 0.0}
--
--
--  points:
-- - !unit/quantity-1.1.0
--   unit: !unit/unit-1.0.0 pixel
--   value: !core/ndarray-1.0.0
--     source: 260
--     datatype: float64
--     byteorder: little
--     shape: [85]
