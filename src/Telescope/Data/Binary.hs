{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Data.Binary where

import Data.Binary.Get
import Data.Binary.Put
import GHC.Int (Int32, Int64)
import System.ByteOrder (ByteOrder (..))


class BinaryValue a where
  byteSize :: Int
  put :: ByteOrder -> a -> Put
  get :: ByteOrder -> Get a


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

-- instance (BinaryValue a) => BinaryValue [a] where
--   byteSize = byteSize @a
--   put bo = mapM_ (put bo)
--   get bo = getToEmpty (get bo)

-- getToEmpty :: Get a -> Get [a]
-- getToEmpty gt = do
--   empty <- isEmpty
--   if empty
--     then return []
--     else do
--       a <- gt
--       as <- getToEmpty gt
--       pure (a : as)