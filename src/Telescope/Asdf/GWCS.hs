{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Telescope.Asdf.GWCS where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, Ix2)
import Data.Massiv.Array qualified as M
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Generics
import Telescope.Asdf
import Telescope.Asdf.Core
import Telescope.Data.WCS (WCSAxis (..))


-- TODO: Affine: Rotation. Matrix, Translation. What is this doing?
-- WARNING: Where does rsun come from in reference frame? Is it possible to do this without duplicating sunpy?

data GWCS inp out = GWCS (GWCSStep inp) (GWCSStep out)
instance (ToAsdf inp, ToAsdf out) => ToAsdf (GWCS inp out) where
  schema _ = "tag:stsci.edu:gwcs/wcs-1.2.0"
  toValue (GWCS inp out) =
    Object
      [ ("name", toNode $ String "")
      , ("steps", toNode $ Array [toNode inp, toNode out])
      ]


data GWCSStep frame = GWCSStep
  { frame :: frame
  , transform :: Maybe Transformation
  }
  deriving (Generic)
instance (ToAsdf frame) => ToAsdf (GWCSStep frame) where
  schema _ = "tag:stsci.edu:gwcs/step-1.1.0"


newtype AxisName = AxisName Text
  deriving newtype (IsString, ToAsdf, Show, Semigroup, Eq)


newtype AxisType = AxisType Text
  deriving newtype (IsString)
instance ToAsdf AxisType where
  toValue (AxisType t) = String t


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


newtype Lon = Lon Float
  deriving newtype (ToAsdf)
newtype Lat = Lat Float
  deriving newtype (ToAsdf)
newtype LonPole = LonPole Float
  deriving newtype (ToAsdf)


data Transformation = Transformation
  { inputs :: [AxisName]
  , outputs :: [AxisName]
  , forward :: Forward
  }
  deriving (Show, Eq)


instance ToAsdf Transformation where
  schema t =
    case t.forward of
      Compose _ _ -> "!transform/compose-1.2.0"
      Concat _ _ -> "!transform/concatenate-1.2.0"
      Direct{schemaTag} -> schemaTag


  toValue t =
    inputFields <> case t.forward of
      Compose t u -> Object [("forward", toNode [t, u])]
      Concat t u -> Object [("forward", toNode [t, u])]
      Direct{fields} -> fields
   where
    inputFields =
      Object
        [ ("inputs", toNode t.inputs)
        , ("outputs", toNode t.outputs)
        ]


data Forward
  = Compose Transformation Transformation
  | Concat Transformation Transformation
  | Direct {schemaTag :: SchemaTag, fields :: Value}
  deriving (Show, Eq)


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
(Transform s) |> (Transform t) =
  Transform
    $ Transformation
      (toAxes @b)
      (toAxes @d)
    $ Compose s t


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
    $ Concat s t
infixr 8 <&>


data Direction
  = Pix2Sky
  | Native2Celestial
  deriving (Show)


instance ToAsdf Direction where
  toValue = String . T.toLower . pack . show


data Shift = Shift Float deriving (Show, Eq)
data Scale = Scale Float deriving (Show, Eq)
data Identity = Identity deriving (Show, Eq)
data Intercept = Intercept Float deriving (Show, Eq)
data Affine = Affine {matrix :: Array M.D Ix2 Float, translation :: (Float, Float)}
data Projection = Projection Direction
data Rotate3d = Rotate3d {direction :: Direction, phi :: Lon, theta :: Lat, psi :: LonPole}
  deriving (Generic)
data Linear1d = Linear1d {intercept :: Float, slope :: Float}
  deriving (Generic)


instance ToAsdf Identity where
  schema _ = "!transform/identity-1.2.0"
  toValue _ = Object []


instance ToAsdf Linear1d where
  schema _ = "!transform/linear1d-1.0.0"


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


instance ToAsdf Affine where
  schema _ = "!transform/affine-1.3.0"
  toValue a =
    let (tx, ty) = a.translation
     in Object
          [ ("matrix", toNode $ M.toLists a.matrix)
          , ("translation", toNode [tx, ty])
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


data StokesFrame = StokesFrame
  { name :: Text
  , axisOrder :: Int
  }
instance ToAsdf StokesFrame where
  schema _ = "tag:stsci.edu:gwcs/stokes_frame-1.0.0"
  toValue f =
    Object
      [ ("name", toNode f.name)
      , ("axes_order", toNode [f.axisOrder])
      ]


data SpectralFrame = SpectralFrame
  { name :: Text
  , axisOrder :: Int
  }
instance ToAsdf SpectralFrame where
  schema _ = "tag:stsci.edu:gwcs/spectral_frame-1.0.0"
  toValue f =
    Object
      [ ("name", toNode f.name)
      , ("axes_names", toNode [String "wavelength"])
      , ("axes_order", toNode [f.axisOrder])
      , ("axis_physical_types", toNode [String "em.wl"])
      , ("unit", toNode [Nanometers])
      ]


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


data CompositeFrame as = CompositeFrame as
instance (ToAsdf as) => ToAsdf (CompositeFrame as) where
  schema _ = "tag:stsci.edu:gwcs/composite_frame-1.0.0"
  toValue (CompositeFrame as) =
    Object
      [ ("name", toNode $ String "CompositeFrame")
      , ("frames", toNode as)
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

shift :: forall a f. (ToAxes (f a), ToAxes (Dlt a)) => Float -> Transform (f a) (Dlt a)
shift d = transform $ Shift d


scale :: forall a f. (ToAxes (f a), ToAxes (Scl a)) => Float -> Transform (f a) (Scl a)
scale d = transform $ Scale d


linear :: forall a. (ToAxes a) => Intercept -> Scale -> Transform (Pix a) (Linear a)
linear (Intercept dlt) (Scale scl) = transform $ Linear1d{intercept = dlt, slope = scl}


rotate :: (ToAxes x, ToAxes y) => Array M.D Ix2 Float -> Transform (Linear x, Linear y) (Rot (x, y))
rotate arr =
  transform $ Affine arr (0, 0)


project :: (ToAxes x, ToAxes y) => Direction -> Transform (Rot (x, y)) (Phi, Theta)
project dir =
  transform $ Projection dir


celestial :: Lat -> Lon -> LonPole -> Transform (Phi, Theta) (Alpha, Delta)
celestial lat lon pole =
  transform $ Rotate3d{direction = Native2Celestial, theta = lat, phi = lon, psi = pole}


identity :: (ToAxes bs, ToAxes cs) => Transform bs cs
identity = transform Identity


-- WCS Transforms ---------------------------------------------------------

wcsLinear :: (ToAxes axis) => WCSAxis alt axis -> Transform (Pix axis) (Linear axis)
wcsLinear wcs = linear (wcsIntercept wcs) (Scale wcs.cdelt)


-- the Y intercept
wcsIntercept :: WCSAxis alt axis -> Intercept
wcsIntercept w =
  -- crpix is 1-indexed, need to switch to zero
  Intercept $ w.crval - w.cdelt * (w.crpix - 1)


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
