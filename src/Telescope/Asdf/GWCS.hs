{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Telescope.Asdf.GWCS where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, Ix1, Ix2)
import Data.Massiv.Array qualified as M
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Generics
import Telescope.Asdf
import Telescope.Asdf.Core

import Data.ByteString qualified as BS
import Telescope.Asdf.Encoding as Encoding


-- TODO: Affine: Rotation. Matrix, Translation. What is this doing?
-- WARNING: Where does rsun come from in reference frame? Is it possible to do this without duplicating sunpy?
--

data GWCSStep frame = GWCSStep
  { frame :: frame
  , transform :: Maybe Transformation
  }
  deriving (Generic)
instance (ToAsdf frame) => ToAsdf (GWCSStep frame) where
  schema _ = "tag:stsci.edu:gwcs/step-1.1.0"


newtype AxisName = AxisName Text
  deriving newtype (IsString, ToAsdf, Show, Semigroup)


newtype AxisType = AxisType Text
  deriving newtype (IsString)
instance ToAsdf AxisType where
  toValue (AxisType t) = String t


data X deriving (Generic, ToAxes)
data Y deriving (Generic, ToAxes)
data Z deriving (Generic, ToAxes)


data Pix a
data Scl a
data Dlt a
data Rot a
data Linear a


instance (ToAxes a) => ToAxes (Pix a) where
  toAxes = toAxes @a
instance (ToAxes a) => ToAxes (Scl a) where
  toAxes = fmap ("*" <>) (toAxes @a)
instance (ToAxes a) => ToAxes (Dlt a) where
  toAxes = fmap ("+" <>) (toAxes @a)
instance (ToAxes a) => ToAxes (Rot a) where
  toAxes = fmap ("rot_" <>) (toAxes @a)
instance (ToAxes a) => ToAxes (Linear a) where
  toAxes = fmap ("lin_" <>) (toAxes @a)


data Phi deriving (Generic, ToAxes)
data Theta deriving (Generic, ToAxes)
data Alpha deriving (Generic, ToAxes)
data Delta deriving (Generic, ToAxes)


newtype Lon = Lon Double
  deriving newtype (ToAsdf)
newtype Lat = Lat Double
  deriving newtype (ToAsdf)
newtype LonPole = LonPole Double
  deriving newtype (ToAsdf)


data Transformation = Transformation
  { inputs :: [AxisName]
  , outputs :: [AxisName]
  , forward :: Forward
  }
  deriving (Show)


instance ToAsdf Transformation where
  schema t =
    case t.forward of
      Compose _ -> "!transform/compose-1.2.0"
      Concat _ -> "!transform/concatenate-1.2.0"
      Direct{schemaTag} -> schemaTag


  toValue t =
    inputFields <> case t.forward of
      Compose ts -> Object [("forward", toNode ts)]
      Concat ts -> Object [("forward", toNode ts)]
      Direct{fields} -> fields
   where
    inputFields =
      Object
        [ ("inputs", toNode t.inputs)
        , ("outputs", toNode t.outputs)
        ]


data Forward
  = Compose (NonEmpty Transformation)
  | Concat (NonEmpty Transformation)
  | Direct {schemaTag :: SchemaTag, fields :: Value}
  deriving (Show)


data Transform b c = Transform
  { transformation :: Transformation
  }
  deriving (Show)


transform :: forall a bs cs. (ToAsdf a, ToAxes bs, ToAxes cs) => a -> Transform bs cs
transform a =
  Transform
    $ Transformation
      (toAxes @bs)
      (toAxes @cs)
    $ Direct (schema a) (toValue a)


(|>) :: forall b c d. (ToAxes b, ToAxes d) => Transform b c -> Transform c d -> Transform b d
(Transform s) |> (Transform t) = Transform
  $ Transformation
    (toAxes @b)
    (toAxes @d)
  $ case t.forward of
    Compose ts -> Compose $ s :| NE.toList ts
    _ -> Compose $ s :| [t]


(<&>)
  :: forall (a :: Type) (b :: Type) (cs :: Type) (ds :: Type)
   . (ToAxes (TConcat a cs), ToAxes (TConcat b ds))
  => Transform a b
  -> Transform cs ds
  -> Transform (TConcat a cs) (TConcat b ds)
Transform s <&> Transform t =
  Transform
    $ Transformation
      (toAxes @(TConcat a cs))
      (toAxes @(TConcat b ds))
    $ concatTransform t.inputs t.forward
 where
  concatTransform [] _ = Concat $ NE.singleton s
  concatTransform _ (Concat ts) = Concat $ s :| NE.toList ts
  concatTransform _ _ = Concat $ s :| [t]
infixr 8 <&>


data Direction
  = Pix2Sky
  | Native2Celestial
  deriving (Show)


instance ToAsdf Direction where
  toValue = String . T.toLower . pack . show


data Shift = Shift Double
data Scale = Scale Double
data Affine = Affine {matrix :: Array M.D Ix2 Double, translation :: (Double, Double)}
data Projection = Projection Direction
data Rotate3d = Rotate3d {direction :: Direction, phi :: Lon, theta :: Lat, psi :: LonPole}


instance ToAsdf Shift where
  schema _ = "!transform/shift-1.2.0"
  toValue (Shift d) =
    Object [("offset", toNode d)]


instance ToAsdf Scale where
  schema _ = "!transform/scale-1.2.0"
  toValue (Scale d) =
    Object [("factor", toNode d)]


instance ToAsdf Projection where
  schema _ = "!transform/gnomonic-1.2.0"
  toValue (Projection d) =
    Object [("direction", toNode d)]


instance ToAsdf Rotate3d where
  schema _ = "!transform/rotate3d-1.3.0"
  toValue r =
    Object
      [ ("direction", toNode r.direction)
      , ("phi", toNode r.phi)
      , ("psi", toNode r.psi)
      , ("theta", toNode r.theta)
      ]


instance ToAsdf Affine where
  schema _ = "!transform/affine-1.3.0"
  toValue a =
    Object
      [ ("matrix", toNode $ toNDArray a.matrix)
      , ("translation", toNode @[Double] [0, 0])
      ]


-- Frames -----------------------------------------------

data CoordinateFrame = CoordinateFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  }
instance ToAsdf CoordinateFrame where
  schema _ = "tag:stsci.edu:gwcs/frame-1.0.0"
  toValue f =
    Object $
      [ ("name", toNode f.name)
      , ("axes_type", toNode $ fmap (.axisType) f.axes)
      ]
        <> frameAxesObject f.axes


data CelestialFrame = CelestialFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  , referenceFrame :: ICRSFrame
  }
instance ToAsdf CelestialFrame where
  schema _ = "tag:stsci.edu:gwcs/celestial_frame-1.0.0"
  toValue f =
    Object $
      [ ("name", toNode f.name)
      , ("reference_frame", toNode f.referenceFrame)
      ]
        <> frameAxesObject f.axes


frameAxesObject :: NonEmpty FrameAxis -> Object
frameAxesObject as =
  -- doesn't include axes_type, only on CoorindateFrame
  [ ("naxes", toNode $ NE.length as)
  , ("axes_names", toNode axesNames)
  , ("axes_order", toNode axesOrders)
  , ("axes_physical_types", toNode axesPhysicalTypes)
  , ("unit", toNode units)
  ]
 where
  axesNames = fmap (.axisName) as
  axesOrders = fmap (.axisOrder) as
  axesPhysicalTypes = fmap (physicalType . (.axisType)) as
  units = fmap (.unit) as
  physicalType t = String "custom:" <> toValue t


-- numAxes = NE.length as

data ICRSFrame = ICRSFrame
instance ToAsdf ICRSFrame where
  schema _ = "tag:astropy.org:astropy/coordinates/frames/icrs-1.1.0"
  toValue _ = Object [("frame_attributes", toNode $ Object mempty)]


data FrameAxis = FrameAxis
  { axisOrder :: Int
  , axisName :: AxisName
  , axisType :: AxisType
  , unit :: Unit
  }


data CompositeFrame a b = CompositeFrame a b
instance (ToAsdf a, ToAsdf b) => ToAsdf (CompositeFrame a b) where
  schema _ = "tag:stsci.edu:gwcs/composite_frame-1.0.0"
  toValue (CompositeFrame a b) =
    Object
      [ ("name", toNode $ String "CompositeFrame")
      , ("frames", toNode $ Array [toNode a, toNode b])
      ]


-- ToAxes -----------------------------------------------

class ToAxes (as :: Type) where
  toAxes :: [AxisName]
  default toAxes :: (Generic as, GTypeName (Rep as)) => [AxisName]
  toAxes = [AxisName $ pack $ gtypeName (from (undefined :: as))]


instance ToAxes () where
  toAxes = []
instance (ToAxes a, ToAxes b) => ToAxes (a, b) where
  toAxes = mconcat [toAxes @a, toAxes @b]
instance (ToAxes a, ToAxes b, ToAxes c) => ToAxes (a, b, c) where
  toAxes = mconcat [toAxes @a, toAxes @b, toAxes @c]
instance (ToAxes a, ToAxes b, ToAxes c, ToAxes d) => ToAxes (a, b, c, d) where
  toAxes = mconcat [toAxes @a, toAxes @b, toAxes @c, toAxes @d]


-- Transforms -----------------------------------------------

shift :: forall a f. (ToAxes (f a), ToAxes (Dlt a)) => Double -> Transform (f a) (Dlt a)
shift d = transform $ Shift d


scale :: forall a f. (ToAxes (f a), ToAxes (Scl a)) => Double -> Transform (f a) (Scl a)
scale d = transform $ Scale d


linear :: forall a. (ToAxes a) => Shift -> Scale -> Transform (Pix a) (Linear a)
linear (Shift dlt) (Scale scl) =
  let t = shift dlt |> scale scl :: Transform (Pix a) (Scl a)
   in Transform $ Transformation (toAxes @a) (toAxes @(Linear a)) t.transformation.forward


rotate :: Array M.D Ix2 Double -> Transform (Linear X, Linear Y) (Rot (X, Y))
rotate arr =
  transform $ Affine arr (0, 0)


project :: Direction -> Transform (Rot (X, Y)) (Phi, Theta)
project dir =
  transform $ Projection dir


celestial :: Lat -> Lon -> LonPole -> Transform (Phi, Theta) (Alpha, Delta)
celestial lat lon pole =
  transform $ Rotate3d{direction = Native2Celestial, theta = lat, phi = lon, psi = pole}


-- | Generic NodeName
class GTypeName f where
  gtypeName :: f p -> String


instance (Datatype d) => GTypeName (D1 d f) where
  gtypeName = datatypeName


type family TConcat a b where
  TConcat (a, b, c) d = (a, b, c, d)
  TConcat a (b, c, d) = (a, b, c, d)
  TConcat (a, b) (c, d) = (a, b, c, d)
  TConcat (a, b) c = (a, b, c)
  TConcat a (b, c) = (a, b, c)
  TConcat a b = (a, b)

-- you can define them inline, but it's gross
-- linearX :: Transform (Pix X) (Linear X)
-- linearX = linear (Shift 10) (Scale 8)
--
--
-- linearY :: Transform (Pix Y) (Linear Y)
-- linearY = linear (Shift 9) (Scale 7)
--
--
-- linearXY :: Transform (Pix X, Pix Y) (Linear X, Linear Y)
-- linearXY = linearX <&> linearY
--
--
-- transformOpticalDepth :: Transform (Pix OpticalDepth) (Linear OpticalDepth)
-- transformOpticalDepth = linear (Shift 8) (Scale 10)
--
--
-- transformComposite :: Transform (Pix OpticalDepth, Pix X, Pix Y) (Linear OpticalDepth, Alpha, Delta)
-- transformComposite = transformOpticalDepth <&> transformSpatial
--

-- test :: IO ()
-- test = do
--   out <- Encoding.encodeM $ Just linearXY.transformation --  Object [("transformation", toNode linearXY.transformation)]
--   BS.writeFile "/Users/seanhess/Downloads/test.asdf" out
--  where
--   inputStep = GWCSStep pixelFrame (Just linearXY.transformation)
--
--   pixelFrame :: CoordinateFrame
--   pixelFrame =
--     CoordinateFrame
--       { name = "pixel"
--       , axes =
--           NE.fromList
--             [ FrameAxis "optical_depth" 0 (AxisType "PIXEL") Pixel
--             , FrameAxis "spatial along slit" 1 (AxisType "PIXEL") Pixel
--             , FrameAxis "raster scan step number" 2 (AxisType "PIXEL") Pixel
--             ]
--       }
--
--   linearX :: Transform (Pix X) (Linear X)
--   linearX = linear (Shift 10) (Scale 8)
--
--   linearXY :: Transform (Pix X, Pix Y) (Linear X, Linear Y)
--   linearXY = linearX <&> linearY
--
--   linearY :: Transform (Pix Y) (Linear Y)
--   linearY = linear (Shift 9) (Scale 7)
