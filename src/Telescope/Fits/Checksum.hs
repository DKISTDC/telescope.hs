module Telescope.Fits.Checksum where

import Data.Bits (complement)
import Data.ByteString.Internal
import Data.Fits (Value (..))
import Data.Text (Text, pack)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import GHC.IO


foreign import ccall "checksum" c_checksum :: Ptr CChar -> CInt -> IO CUInt


newtype Checksum = Checksum Word32
  deriving (Eq, Show)


checksum :: ByteString -> Checksum
checksum bs = unsafePerformIO $ do
  let (fptr, offset, len) = toForeignPtr bs
  withForeignPtr fptr $ \ptr -> do
    ci <- c_checksum (ptr `plusPtr` offset) (fromIntegral len)
    pure $ Checksum $ fromIntegral ci


checksumValue :: Checksum -> Value
checksumValue (Checksum s) = String (pack (show s))



foreign import ccall "char_encode" char_encode :: CUInt -> CString -> IO ()


encodeChecksum :: Checksum -> Text
encodeChecksum (Checksum csum) =
  unsafePerformIO $ do
    let comp = complement csum
    let str = replicate 16 ' '
    out <- withCString str $ \cs -> do
      char_encode (fromIntegral comp) cs
      peekCString cs
    pure $ pack out
