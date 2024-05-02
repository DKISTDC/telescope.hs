module Telescope.Fits.Checksum where

import Data.Binary.Get
import Data.Bits
import Data.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Internal
import Data.Foldable (foldl', foldr')
import Data.Text (Text, pack)
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.IO
import Numeric


-- add1s :: forall a. (Bounded a, Bits a, FiniteBits a, Integral a) => a -> a -> a
-- add1s x y =
--   let
--     sum64 = (fromIntegral x :: Word64) + fromIntegral y
--     result =
--       if sum64 > maxWord
--         then (sum64 .&. maxWord) + (sum64 `shiftR` finiteBitSize x)
--         else sum64
--    in
--     fromIntegral result
--  where
--   maxWord :: Word64
--   maxWord = fromIntegral (maxBound :: a)
--
--
-- -- Faster?
-- add1s' :: Word32 -> Word32 -> Word32
-- add1s' x y =
--   let
--     sum64 = (+) @Word64 (fromIntegral x) (fromIntegral y)
--     result =
--       if sum64 > maxWord32
--         then (sum64 .&. maxWord32) + (sum64 `shiftR` 32)
--         else sum64
--    in
--     fromIntegral result
--
--
-- maxWord32 :: Word64
-- maxWord32 = 0xFFFFFFF -- fromIntegral (maxBound :: Word32)
--
--
-- sum32 :: [Word32] -> Word32
-- -- sum32 = foldl' add1s' 0
-- sum32 = foldl' f 0
--   where
--     f !a !b = a + b
--
--
-- -- not sure this is right...
-- words32 :: BS.ByteString -> [Word32]
-- words32 s = fmap fromChunk32 $ allChunks $ BS.unpack s
--  where
--   allChunks :: [Word8] -> [(Word8, Word8, Word8, Word8)]
--   allChunks (w1 : w2 : w3 : w4 : ws) =
--     (w1, w2, w3, w4) : allChunks ws
--   allChunks _ = []
--
--   fromChunk32 :: (Word8, Word8, Word8, Word8) -> Word32
--   fromChunk32 (a, b, c, d) =
--     fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d
--
--
-- datasum :: BS.ByteString -> Text
-- datasum = pack . show . sum32 . words32

foreign import ccall "checksum" c_checksum :: Ptr CChar -> CInt -> IO CUInt


checksum :: ByteString -> Word32
checksum bs = unsafePerformIO $ do
  let (fptr, offset, len) = toForeignPtr bs
  withForeignPtr fptr $ \ptr -> do
    ci <- c_checksum (ptr `plusPtr` offset) (fromIntegral len)
    pure $ fromIntegral ci
