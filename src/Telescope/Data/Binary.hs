{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.Binary
  ( BinaryValue (..)
  , ByteOrder (..)
  ) where

import Data.Binary.Get
import Data.Binary.Put
import GHC.Int
import System.ByteOrder (ByteOrder (..))


class BinaryValue a where
  byteSize :: Int
  put :: ByteOrder -> a -> Put
  get :: ByteOrder -> Get a


instance BinaryValue Int8 where
  byteSize = 1
  put _ = putInt8
  get _ = getInt8


instance BinaryValue Int16 where
  byteSize = 2
  put BigEndian = putInt16be
  put LittleEndian = putInt16le
  get BigEndian = getInt16be
  get LittleEndian = getInt16le


instance BinaryValue Int32 where
  byteSize = 4
  put BigEndian = putInt32be
  put LittleEndian = putInt32le
  get BigEndian = getInt32be
  get LittleEndian = getInt32le


instance BinaryValue Int64 where
  byteSize = 8
  put BigEndian = putInt64be
  put LittleEndian = putInt64le
  get BigEndian = getInt64be
  get LittleEndian = getInt64le


instance BinaryValue Int where
  byteSize = byteSize @Int64
  put bo a = put @Int64 bo (fromIntegral a)
  get bo = fromIntegral <$> get @Int64 bo


instance BinaryValue Float where
  byteSize = 4
  put BigEndian = putFloatbe
  put LittleEndian = putFloatle
  get BigEndian = getFloatbe
  get LittleEndian = getFloatle


instance BinaryValue Double where
  byteSize = 8
  put BigEndian = putDoublebe
  put LittleEndian = putDoublele
  get BigEndian = getDoublebe
  get LittleEndian = getDoublele
