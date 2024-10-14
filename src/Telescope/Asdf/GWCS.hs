{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Telescope.Asdf.GWCS where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Massiv.Array (Array, Ix1, Ix2)
import Data.Massiv.Array qualified as M
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

data GWCSStep frame inp out = GWCSStep
  { frame :: frame
  , transform :: Maybe (Transform inp out)
  }


instance (ToAsdf frame, ToAsdf (Transform inp out)) => ToAsdf (GWCSStep frame inp out) where
  schema _ = "tag:stsci.edu:gwcs/step-1.1.0"
  toValue step =
    Object
      [ ("frame", toNode step.frame)
      , ("transform", toNode step.transform)
      ]


newtype AxisName = AxisName Text
  deriving newtype (IsString, ToAsdf, Show, Semigroup)


newtype AxisType = AxisType Text
instance ToAsdf AxisType where
  toValue (AxisType t) = String t


data CoordinateFrame = CoordinateFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  }


instance ToAsdf CoordinateFrame where
  schema _ = "tag:stsci.edu:gwcs/frame-1.0.0"
  toValue f =
    Object $ [("name", toNode f.name)] <> frameAxesObject f.axes


frameAxesObject :: NonEmpty FrameAxis -> Object
frameAxesObject as =
  [ ("naxes", toNode $ NE.length as)
  , ("axes_names", toNode axesNames)
  , ("axes_order", toNode axesOrders)
  , ("axes_type", toNode axesTypes)
  , ("axes_physical_types", toNode axesPhysicalTypes)
  , ("unit", toNode units)
  ]
 where
  axesNames = fmap (.axisName) as
  axesOrders = [0 .. (numAxes - 1)]
  axesTypes = fmap (.axisType) as
  axesPhysicalTypes = fmap (physicalType . (.axisType)) as
  units = fmap (.axisName) as
  physicalType t = String "custom:" <> toValue t
  numAxes = NE.length as


data CelestialFrame = CelestialFrame
  { name :: Text
  , axes :: NonEmpty FrameAxis
  , referenceFrame :: ICRSFrame
  }


instance ToAsdf CelestialFrame where
  schema _ = "tag:stsci.edu:gwcs/celestial_frame-1.0.0"
  toValue f =
    Object $ [("name", toNode f.name), ("reference_frame", toNode f.referenceFrame)] <> frameAxesObject f.axes


data ICRSFrame = ICRSFrame
instance ToAsdf ICRSFrame where
  schema _ = "tag:astropy.org:astropy/coordinates/frames/icrs-1.1.0"
  toValue _ = Object [("frame_attributes", toNode $ Object mempty)]


data FrameAxis = FrameAxis
  { axisName :: AxisName
  , axisOrder :: Int
  , axisType :: AxisType
  , unit :: Unit
  }


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


data Forward
  = Compose (NonEmpty Transformation)
  | Concat (NonEmpty Transformation)
  | Direct {schemaTag :: SchemaTag, fields :: Value}
  deriving (Show)


data Transform b c = Transform
  { transformation :: Transformation
  }
  deriving (Show)


-- toTransform :: forall a b c. (ToAsdf a, ToAxis b, ToAxis c) => a -> Transform b c
-- toTransform a =
--   Transform
--     $ Transformation
--       [toAxis @b]
--       [toAxis @c]
--     $ Direct (schema a) (toValue a)

transform :: forall a bs cs. (ToAsdf a, ToAxes bs, ToAxes cs) => a -> Transform bs cs
transform a =
  Transform
    $ Transformation
      (toAxes @bs)
      (toAxes @cs)
    $ Direct (schema a) (toValue a)


instance (ToAxes b, ToAxes c) => ToAsdf (Transform b c) where
  schema (Transform t) = schema t
  toValue (Transform t) = toValue t


instance ToAsdf Transformation where
  schema t =
    case t.forward of
      Compose _ -> "transform/compose-1.2.0"
      Concat _ -> "transform/concatenate-1.2.0"
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
  schema _ = "transform/shift-1.2.0"
  toValue (Shift d) =
    Object [("shift", toNode d)]


instance ToAsdf Scale where
  schema _ = "transform/scale-1.2.0"
  toValue (Scale d) =
    Object [("scale", toNode d)]


instance ToAsdf Projection where
  schema _ = "transform/gnomonic-1.2.0"
  toValue (Projection d) =
    Object [("direction", toNode d)]


instance ToAsdf Rotate3d where
  schema _ = "transform/rotate3d-1.3.0"
  toValue r =
    Object
      [ ("direction", toNode r.direction)
      , ("phi", toNode r.phi)
      , ("psi", toNode r.psi)
      , ("theta", toNode r.theta)
      ]


instance ToAsdf Affine where
  schema _ = "transform/affine-1.3.0"
  toValue a =
    Object
      [ ("matrix", toNode $ toNDArray a.matrix)
      , ("translation", toNode @[Double] [0, 0])
      ]


data CompositeFrame a b = CompositeFrame a b


instance (ToAsdf a, ToAsdf b) => ToAsdf (CompositeFrame a b) where
  schema _ = "tag:stsci.edu:gwcs/composite_frame-1.0.0"
  toValue (CompositeFrame a b) =
    Object
      [("frames", toNode $ Array [toNode a, toNode b])]


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


-- data OpticalDepth deriving (Generic, ToAxes)
--
--
-- transformSpatial :: Transform (Pix X, Pix Y) (Alpha, Delta)
-- transformSpatial = linearXY |> rotate pcMatrix |> project Pix2Sky |> celestial (Lat 1) (Lon 2) (LonPole 180)
--  where
--   pcMatrix :: Array M.D Ix2 Double
--   pcMatrix = M.delay $ M.fromLists' @M.P M.Seq [[0, 1], [2, 3]]

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

-- test :: IO ()
-- test = do
--   out <- Encoding.encodeM $ Object [("transform", toNode transformComposite)]
--   BS.writeFile "/Users/seanhess/Downloads/test.asdf" out

{-

Created in python:
 -
wcs: !<tag:stsci.edu:gwcs/wcs-1.2.0>
  name: ''
  pixel_shape: null
  steps:
  - !<tag:stsci.edu:gwcs/step-1.1.0>
    frame: !<tag:stsci.edu:gwcs/frame-1.0.0>
      axes_names: [optical_depth, spatial along slit, raster scan step number]
      axes_order: [0, 1, 2]
      axes_type: [PIXEL, PIXEL, PIXEL]
      axis_physical_types: ['custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL']
      name: pixel
      naxes: 3
      unit: [!unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel]
    transform: !transform/concatenate-1.2.0
      forward:
      - !transform/compose-1.2.0
        forward:
        - !transform/shift-1.2.0
          inputs: [x]
          offset: -11.0
          outputs: [y]
        - !transform/scale-1.2.0
          factor: 0.1
          inputs: [x]
          outputs: [y]
        inputs: [x]
        outputs: [y]
      - !transform/compose-1.2.0
        forward:
        - !transform/compose-1.2.0
          forward:
          - !transform/compose-1.2.0
            forward:
            - !transform/compose-1.2.0
              forward:
              - !transform/concatenate-1.2.0
                forward:
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: -27.571428
                  outputs: [y]
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: -13.520178
                  outputs: [y]
                inputs: [x0, x1]
                outputs: [y0, y1]
              - !transform/concatenate-1.2.0
                forward:
                - !transform/scale-1.2.0
                  factor: 0.000415055
                  inputs: [x]
                  outputs: [y]
                - !transform/shift-1.2.0
                  inputs: [x]
                  offset: 5.929356944444445e-05
                  outputs: [y]
                inputs: [x0, x1]
                outputs: [y0, y1]
              inputs: [x0, x1]
              outputs: [y0, y1]
            - !transform/affine-1.3.0
              inputs: [x, y]
              matrix: !core/ndarray-1.0.0
                source: 0
                datatype: float64
                byteorder: little
                shape: [2, 2]
              outputs: [x, y]
              translation: !core/ndarray-1.0.0
                source: 1
                datatype: float64
                byteorder: little
                shape: [2]
            inputs: [x0, x1]
            outputs: [x, y]
          - !transform/gnomonic-1.2.0
            direction: pix2sky
            inputs: [x, y]
            outputs: [phi, theta]
          inputs: [x0, x1]
          outputs: [phi, theta]
        - !transform/rotate3d-1.3.0
          direction: native2celestial
          inputs: [phi_N, theta_N]
          outputs: [alpha_C, delta_C]
          phi: -0.1333360111111111
          psi: 180.0
          theta: -0.1130558888888889
        inputs: [x0, x1]
        outputs: [alpha_C, delta_C]
      inputs: [x, x0, x1]
      outputs: [y, alpha_C, delta_C]
  - !<tag:stsci.edu:gwcs/step-1.1.0>
    frame: !<tag:stsci.edu:gwcs/composite_frame-1.0.0>
      frames:
      - !<tag:stsci.edu:gwcs/frame-1.0.0>
        axes_names: [optical_depth]
        axes_order: [0]
        axes_type: [optical_depth]
        axis_physical_types: ['custom:optical_depth']
        name: optical_depth_out
        naxes: 1
        unit: [!unit/unit-1.0.0 pixel]
      - !<tag:stsci.edu:gwcs/celestial_frame-1.0.0>
        axes_names: [helioprojective longitude, helioprojective latitude]
        axes_order: [1, 2]
        axis_physical_types: ['custom:pos.helioprojective.lon', 'custom:pos.helioprojective.lat']
        name: helioprojective
        reference_frame: !<tag:sunpy.org:sunpy/coordinates/frames/helioprojective-1.0.0>
          frame_attributes:
            observer: !<tag:sunpy.org:sunpy/coordinates/frames/heliographic_stonyhurst-1.1.0>
              data: !<tag:astropy.org:astropy/coordinates/representation-1.1.0>
                components:
                  x: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: 151718470759.01736}
                  y: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: 936374.8961084613}
                  z: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 m,
                    value: -1238552794.1080718}
                type: CartesianRepresentation
              frame_attributes:
                obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
                rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
                  value: 695700.0}
            obstime: !time/time-1.1.0 2022-06-02T21:47:26.641
            rsun: !unit/quantity-1.1.0 {datatype: float64, unit: !unit/unit-1.0.0 km,
              value: 695700.0}
        unit: [!unit/unit-1.0.0 deg, !unit/unit-1.0.0 deg]
      name: CompositeFrame
    transform: null
-}
