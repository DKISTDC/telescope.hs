{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

module Telescope.Asdf.GWCS where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
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
  { name :: Text
  , frame :: frame
  , transform :: Transform inp out
  }


instance (ToAsdf frame, ToAsdf (Transform inp out)) => ToAsdf (GWCSStep frame inp out) where
  toValue step =
    Object
      [ ("name", toNode step.name)
      , ("frame", toNode step.frame)
      , ("transform", toNode step.transform)
      ]


newtype AxisName = AxisName Text
  deriving newtype (IsString, ToAsdf, Show, Semigroup)


data AxisType = AxisPixel
instance ToAsdf AxisType where
  toValue AxisPixel = String "PIXEL"


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


data X deriving (Generic, ToAxis)
data Y deriving (Generic, ToAxis)


data Pix a
data Scl a
data Dlt a
data Rot a


instance (ToAxis a) => ToAxis (Pix a) where
  toAxis = "pix " <> toAxis @a
instance (ToAxis a) => ToAxis (Scl a) where
  toAxis = "scl " <> toAxis @a
instance (ToAxis a) => ToAxis (Dlt a) where
  toAxis = "dlt " <> toAxis @a
instance (ToAxis a) => ToAxis (Rot a) where
  toAxis = "rot " <> toAxis @a


data Phi deriving (Generic, ToAxis)
data Theta deriving (Generic, ToAxis)
data Alpha deriving (Generic, ToAxis)
data Delta deriving (Generic, ToAxis)


empty :: Transform '[] '[]
empty = Transform $ Transformation [] [] (Direct mempty mempty)


-- you can't shift anything. It has to NOT be a
shift :: (ToAxes '[f a], ToAxes '[Dlt a]) => Double -> Transform (f a) (Dlt a)
shift d = toTransform $ Shift d


scale :: (ToAxes '[f a], ToAxes '[Scl a]) => Double -> Transform (f a) (Scl a)
scale d = toTransform $ Scale d


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


-- this only works for singles
toTransform :: forall a b c. (ToAsdf a, ToAxes '[b], ToAxes '[c]) => a -> Transform b c
toTransform a =
  Transform
    $ Transformation
      (toAxes @'[b])
      (toAxes @'[c])
    $ Direct (schema a) (toValue a)


toTransforms :: forall a bs cs. (ToAsdf a, ToAxes bs, ToAxes cs) => a -> Transform bs cs
toTransforms a =
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


(<:>)
  :: forall (a :: Type) (b :: Type) (cs :: [Type]) (ds :: [Type])
   . (ToAxes (a : cs), ToAxes (b : ds))
  => Transform a b
  -> Transform cs ds
  -> Transform (a : cs) (b : ds)
Transform s <:> Transform t =
  Transform
    $ Transformation
      (toAxes @(a : cs))
      (toAxes @(b : ds))
    $ concatTransform t.inputs t.forward
 where
  concatTransform [] _ = Concat $ NE.singleton s
  concatTransform _ (Concat ts) = Concat $ s :| NE.toList ts
  concatTransform _ _ = Concat $ s :| [t]
infixr 8 <:>


data Direction
  = Pix2Sky
  | Native2Celestial
  deriving (Show)


instance ToAsdf Direction where
  toValue = String . T.toLower . pack . show


data Shift = Shift Double
data Scale = Scale Double
data Affine = Affine {matrix :: NDArrayData, translation :: NDArrayData}
data Skip = Skip
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


instance ToAsdf Skip where
  toValue _ = Object []


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


newtype CompositeFrame = CompositeFrame (NonEmpty CoordinateFrame)


-- data ReferenceFrame = ReferenceFrame
--   { observer :: _
--   }

class ToAxis (a :: Type) where
  toAxis :: AxisName
  default toAxis :: (Generic a, GTypeName (Rep a)) => AxisName
  toAxis = AxisName $ pack $ gtypeName (from (undefined :: a))


class ToAxes (as :: [Type]) where
  toAxes :: [AxisName]


instance (ToAxis a) => ToAxes '[a] where
  toAxes = [toAxis @a]
instance (ToAxes '[a], ToAxes '[b]) => ToAxes [a, b] where
  toAxes = toAxes @'[a] <> toAxes @'[b]
instance (ToAxes '[a], ToAxes '[b], ToAxes '[c]) => ToAxes [a, b, c] where
  toAxes = mconcat [toAxes @'[a], toAxes @'[b], toAxes @'[c]]
instance (ToAxes '[a], ToAxes '[b], ToAxes '[c], ToAxes '[d]) => ToAxes [a, b, c, d] where
  toAxes = mconcat [toAxes @'[a], toAxes @'[b], toAxes @'[c], toAxes @'[d]]


-- | Generic NodeName
class GTypeName f where
  gtypeName :: f p -> String


instance (Datatype d) => GTypeName (D1 d f) where
  gtypeName = datatypeName


data OpticalDepth deriving (Generic, ToAxis)


transformSpatial :: Transform [Pix X, Pix Y] [Alpha, Delta]
transformSpatial = shiftXY |> scaleXY |> rotate |> projection |> celestial
 where
  -- TODO: is this correct? I'm confused about the angles, etc
  rotate :: Transform [Scl X, Scl Y] [Rot X, Rot Y]
  rotate = toTransform Skip <:> toTransform Skip <:> empty

  projection :: (ToAxis (f X), ToAxis (f Y)) => Transform '[f X, f Y] [Phi, Theta]
  projection =
    toTransforms $ Projection Pix2Sky

  celestial :: Transform [Phi, Theta] [Alpha, Delta]
  celestial =
    toTransforms $ Rotate3d{direction = Native2Celestial, theta = Lat (-0.1130558888888889), phi = Lon (-0.1333360111111111), psi = LonPole 180.0}


shiftXY :: Transform [Pix X, Pix Y] [Dlt X, Dlt Y]
shiftXY = shift 6 <:> shift 8 <:> empty


scaleXY :: Transform [Dlt X, Dlt Y] [Scl X, Scl Y]
scaleXY = scale 7 <:> scale 8 <:> empty


transformOpticalDepth :: Transform [Pix OpticalDepth] [Scl OpticalDepth]
transformOpticalDepth = (shift 8 <:> empty) |> (scale 10 <:> empty)


transformComposite :: Transform [Pix OpticalDepth, Pix X, Pix Y] [Scl OpticalDepth, Alpha, Delta]
transformComposite = transformOpticalDepth <:> transformSpatial


test :: IO ()
test = do
  out <- Encoding.encodeM $ Object [("transform", toNode shiftXY)]
  BS.writeFile "/Users/seanhess/Downloads/test.asdf" out

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
