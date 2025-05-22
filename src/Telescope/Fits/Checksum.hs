module Telescope.Fits.Checksum where

import Data.Bits (complement, shiftR, (.&.))
import Data.ByteString.Internal
import Data.Text (Text, pack)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import GHC.IO
import Telescope.Fits.Header.Value


-- | Generate the Checksum per the FITS spec
checksum :: ByteString -> Checksum
checksum bs = unsafePerformIO $ do
  let (fptr, offset, len) = toForeignPtr bs
  withForeignPtr fptr $ \ptr -> do
    ci <- c_checksum (ptr `plusPtr` offset) (fromIntegral len)
    pure $ Checksum $ fromIntegral ci


-- | Encode the Checksum as ASCII chars per FITS spec
encodeChecksum :: Checksum -> Text
encodeChecksum (Checksum csum) =
  unsafePerformIO $ do
    let comp = complement csum
    let str = replicate 16 ' '
    out <- withCString str $ \cs -> do
      c_char_encode (fromIntegral comp) cs
      peekCString cs
    pure $ pack out


foreign import ccall "checksum" c_checksum :: Ptr CChar -> CInt -> IO CUInt


newtype Checksum = Checksum Word32
  deriving (Eq, Show)


instance Semigroup Checksum where
  Checksum w1 <> Checksum w2 = Checksum (add1s w1 w2)


checksumValue :: Checksum -> Value
checksumValue (Checksum s) = String (pack (show s))


foreign import ccall "char_encode" c_char_encode :: CUInt -> CString -> IO ()


add1s :: Word32 -> Word32 -> Word32
add1s x y =
  let
    sum64 = (+) @Word64 (fromIntegral x) (fromIntegral y)
    result =
      if sum64 > maxWord32
        then (sum64 .&. maxWord32) + (sum64 `shiftR` 32)
        else sum64
   in
    fromIntegral result
 where
  maxWord32 :: Word64
  maxWord32 = 0xFFFFFFFF
