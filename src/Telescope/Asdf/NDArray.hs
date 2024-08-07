module Telescope.Asdf.NDArray where


-- https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/ndarray-1.1.0.html

-- TODO: needs to be able to serialize the "datatype" to a certain value

-- * easy to do with this type, hard to do with shared binary type


data Scalar
  = SInt8
  | SInt16
  | SInt32
  | SInt64
  | SUInt8
  | SUInt16
  | SUInt32
  | SUInt64
  | SFloat16
  | SFloat32
  | SFloat64
  | SComplex64
  | SComplex128
  | SBool8
  | SAscii Length
  | SUCS4 Length


type Length = Int


data DataType
  = Scalar Scalar
