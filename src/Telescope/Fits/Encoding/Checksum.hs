module Telescope.Fits.Encoding.Checksum where

import Data.Binary.Get
import Data.Bits
import Data.ByteString qualified as BS
import Data.Text (pack, Text)
import Data.Word
import Numeric


add1s :: forall a. (Bounded a, Bits a, FiniteBits a, Integral a) => a -> a -> a
add1s x y =
  let
    sum64 = (fromIntegral x :: Word64) + fromIntegral y
    result =
      if sum64 > maxWord
        then (sum64 .&. maxWord) + (sum64 `shiftR` finiteBitSize x)
        else sum64
   in
    fromIntegral result
 where
  maxWord :: Word64
  maxWord = fromIntegral (maxBound :: a)


-- Faster?
add1s' :: Word32 -> Word32 -> Word32
add1s' x y =
  let
    sum64 = (+) @Word64 (fromIntegral x) (fromIntegral y)
    result =
      if sum64 > maxWord32
        then (sum64 .&. maxWord32) + (sum64 `shiftR` 32)
        else sum64
   in
    fromIntegral result


maxWord32 :: Word64
maxWord32 = fromIntegral (maxBound :: Word32)


sum32 :: [Word32] -> Word32
sum32 = foldl add1s' 0


-- not sure this is right... 
words32 :: BS.ByteString -> [Word32]
words32 s = fmap fromChunk32 $ allChunks $ BS.unpack s
 where
  allChunks :: [Word8] -> [(Word8, Word8, Word8, Word8)]
  allChunks (w1 : w2 : w3 : w4 : ws) =
    (w1, w2, w3, w4) : allChunks ws
  allChunks _ = []

  fromChunk32 :: (Word8, Word8, Word8, Word8) -> Word32
  fromChunk32 (a, b, c, d) =
    fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d


datasum :: BS.ByteString -> Text
datasum = pack . show . sum32 . words32

-- import Data.Bits (complement, shiftL, (.&.), (.|.))
-- import Data.ByteString qualified as BS
-- import Data.ByteString.Internal (w2c)
-- import Data.Word (Word32, Word8)
--
--
-- -- Convert four bytes to a single Word32 (big endian)
-- bytesToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
-- bytesToWord32 a b c d = fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d
--
--
-- -- Calculate the checksum for a single 2880-byte block
-- calculateBlockChecksum :: BS.ByteString -> Word32
-- calculateBlockChecksum block = BS.foldl' addComplement 0 words
--  where
--   bytes = BS.unpack block
--   words = [bytesToWord32 (bytes !! (i * 4)) (bytes !! (i * 4 + 1)) (bytes !! (i * 4 + 2)) (bytes !! (i * 4 + 3)) | i <- [0 .. 719]]
--   addComplement acc word = (acc + word) .&. 0xFFFFFFFF
--
--
-- -- Ones' complement of the checksum
-- onesComplement :: Word32 -> Word32
-- onesComplement = complement
--
--
-- -- Main function to process the entire ByteString
-- calculateChecksum :: BS.ByteString -> Word32
-- calculateChecksum bs = onesComplement . BS.foldl' (\acc block -> (acc + calculateBlockChecksum block) .&. 0xFFFFFFFF) 0 $ BS.chunksOf 2880 bs
--
--
-- main :: IO ()
-- main = do
--   content <- BS.readFile "your_fits_file.fits" -- Update this with your actual file path
--   let checksum = calculateChecksum content
--   putStrLn $ "Checksum: " ++ show checksum
