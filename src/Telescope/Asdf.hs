{- |
Module:      Telescope.Asdf
Copyright:   (c) 2024 Sean Hess
License:     BSD3
Maintainer:  Sean Hess <shess@nso.edu>
Stability:   experimental
Portability: portable

Read, and Write ASDF (Advanced Scientific Data Format) files

> import Data.ByteString qualified as BS
> import Telescope.Asdf
>
> data Example = Example
>   { name :: Text
>   , items :: [Text]
>   , sequence :: [Int64]
>   , random :: Array D Ix1 Double
>   }
>   deriving (Generic, FromAsdf)
>
> example :: IO ()
> example = do
>   inp <- BS.readFile "samples/example.asdf"
>   ex :: Example <- decodeM inp
>   print ex.name
>   print ex.items
>   print $ take 30 ex.sequence
>   print $ take 10 $ M.toList ex.random
-}
module Telescope.Asdf
  ( -- * Encoding
    encodeM
  , encode
  , ToAsdf (..)

    -- * Decoding
  , decodeM
  , decodeEither
  , decode
  , FromAsdf (..)
  , (.:)
  , (.:?)
  , AsdfError
  , Parser

    -- * Binary Data
  , FromNDArray (..)
  , ToNDArray (..)
  , NDArrayData (..)

    -- * ASDF Tree
  , Asdf (..)
  , Node (..)
  , Value (..)
  , Key
  , Object
  , fromValue
  , SchemaTag

    -- * JSON Reference
  , jsonPointer
  , jsonReference
  , JSONReference (..)
  , JSONPointer (..)

    -- * YAML Anchors
  , Anchor (..)

    -- ** Exports
  , Generic
  )
where

import GHC.Generics
import Telescope.Asdf.Class
import Telescope.Asdf.Core (Asdf (..))
import Telescope.Asdf.Encoding
import Telescope.Asdf.Error
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node

