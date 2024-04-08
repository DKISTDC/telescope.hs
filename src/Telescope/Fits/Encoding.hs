module Telescope.Fits.Encoding where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString qualified as BS
import Data.Fits qualified as Fits
import Data.Fits.MegaParser qualified as Fits
import Data.Fits.Read (FitsError (..))
import Telescope.Fits.Types
import Text.Megaparsec qualified as M

import Control.Monad (replicateM_)
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Char (toUpper)
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Telescope.Fits.DataArray
import Telescope.Fits.Types


decode :: forall m. (MonadThrow m) => BS.ByteString -> m Fits
decode inp = do
  hdus <- either (throwM . FormatError . ParseError) pure $ M.runParser Fits.parseHDUs "FITS" inp
  case hdus of
    [] -> throwM MissingPrimary
    (h : hs) -> do
      primaryHDU <- toPrimary h
      extensions <- mapM toExtension hs
      pure $ Fits{primaryHDU, extensions}
 where
  toExtension :: Fits.HeaderDataUnit -> m Extension
  toExtension hdu =
    case hdu._extension of
      Fits.Primary -> throwM $ InvalidExtension "Primary, expected Extension"
      Fits.Image -> pure $ Image $ ImageHDU hdu._header $ dataArray hdu
      ex -> throwM $ InvalidExtension (show ex)

  toPrimary :: Fits.HeaderDataUnit -> m PrimaryHDU
  toPrimary hdu =
    case hdu._extension of
      Fits.Primary -> pure $ PrimaryHDU hdu._header $ dataArray hdu
      _ -> throwM $ InvalidExtension "Extension, expected Primary"

  dataArray :: Fits.HeaderDataUnit -> DataArray
  dataArray hdu =
    DataArray
      { bitpix = bitpix hdu._dimensions._bitpix
      , axes = axes hdu._dimensions._axes
      , rawData = hdu._mainData
      }

  -- toImage :: Fits.HeaderDataUnit -> m ImageHDU

  bitpix :: Fits.BitPixFormat -> BitPix
  bitpix Fits.EightBitInt = BPInt8
  bitpix Fits.SixteenBitInt = BPInt16
  bitpix Fits.ThirtyTwoBitInt = BPInt32
  bitpix Fits.SixtyFourBitInt = BPInt64
  bitpix Fits.ThirtyTwoBitFloat = BPFloat
  bitpix Fits.SixtyFourBitFloat = BPDouble

  axes :: Fits.Axes -> Axes Column
  axes = Axes


data HDUError
  = InvalidExtension String
  | MissingPrimary
  | FormatError FitsError
  deriving (Show, Exception)


-- encode :: Fits -> BS.ByteString
-- encode = _
--
--

-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE NoFieldSelectors #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- module Data.Fits.Write where
--

{- | Encode HeaderDataUnits into a FITS file

>>> import qualified Data.ByteString.Lazy as BL
>>> BL.writeFile "myfile.fits" $ encodeHDUs hdus
encodeHDUs :: [HeaderDataUnit] -> BL.ByteString
encodeHDUs [] = error "encodeHDUs: []"
encodeHDUs hdus = _
-}

-- | Execute a BuilderBlock and create a bytestring
runRender :: BuilderBlock -> BL.ByteString
runRender bb = toLazyByteString bb.builder


-- renderHDU :: HeaderDataUnit -> BuilderBlock
-- renderHDU hdu = renderHeader hdu <> renderData hdu._mainData

renderData :: BL.ByteString -> BuilderBlock
renderData s = fillBlock $ BuilderBlock (fromIntegral $ BL.length s) $ lazyByteString s


-- renderHeader :: HeaderDataUnit -> BuilderBlock
-- renderHeader hdu =
--   fillBlock $
--     mconcat
--       [ renderHDUType hdu._extension
--       , -- what if I don't set these. They are optional. Not sure if astro.py will be angry. I'm not sure they have very much valeu
--         -- TODO: DATASUM - update first
--         -- TODO: CHECKSUM - update very last
--         renderKeywordLine "CHECKSUM" (String "TODO") Nothing
--       , renderKeywordLine "DATASUM" (String "TODO") Nothing
--       , renderOtherKeywords hdu._header
--       , renderEnd
--       ]
--  where
--   renderHDUType Primary = renderPrimaryHeader hdu._dimensions
--   renderHDUType Image = renderImageHeader hdu._dimensions
--   -- WARNING: not supported yet
--   renderHDUType _ = ""
--
--   renderEnd = pad 80 "END"

renderImageHeader :: BitPix -> Axes Column -> BuilderBlock
renderImageHeader bp as =
  mconcat
    [ renderKeywordLine "XTENSION" (String "IMAGE") (Just "Image Extension")
    , renderImageKeywords bp as
    ]


renderPrimaryHeader :: BitPix -> Axes Column -> BuilderBlock
renderPrimaryHeader bp as =
  mconcat
    [ renderKeywordLine "SIMPLE" (Logic T) (Just "Conforms to the FITS standard")
    , renderImageKeywords bp as
    , renderKeywordLine "EXTEND" (Logic T) Nothing
    ]


renderImageKeywords :: BitPix -> Axes Column -> BuilderBlock
renderImageKeywords bp (Axes as) =
  mconcat
    [ bitpix
    , naxis_
    , naxes
    ]
 where
  bitpix = renderKeywordLine "BITPIX" (Integer $ bitpixCode bp) (Just "array data type")
  naxis_ = renderKeywordLine "NAXIS" (Integer $ length as) Nothing
  naxes = mconcat $ zipWith naxisN [1 ..] as
  naxisN n a =
    renderKeywordLine (Keyword $ "NAXIS" <> pack (show n)) (Integer a) Nothing


-- | 'Header' should contain only extra keywords. The system will generate all mandatory keywords
renderOtherKeywords :: Header -> BuilderBlock
renderOtherKeywords (Header ks) =
  mconcat $ map (\(k, v) -> renderKeywordLine k v Nothing) ks


-- | Fill out the header or data block to the nearest 2880 bytes
fillBlock :: BuilderBlock -> BuilderBlock
fillBlock b =
  let rem = hduBlockSize - b.length `mod` hduBlockSize
   in b <> extraSpaces rem
 where
  extraSpaces n
    | n == hduBlockSize = mempty
    | otherwise = spaces n


bitpixCode :: BitPix -> Int
bitpixCode BPInt8 = 8
bitpixCode BPInt16 = 16
bitpixCode BPInt32 = 32
bitpixCode BPInt64 = 64
bitpixCode BPFloat = -32
bitpixCode BPDouble = -64


-- Keyword Lines -----------------------------------------------------

renderKeywordLine :: Keyword -> Value -> Maybe Comment -> BuilderBlock
renderKeywordLine k v mc =
  let kv = renderKeywordValue k v
   in pad 80 $ addComment kv mc
 where
  addComment kv Nothing = kv
  addComment kv (Just c) =
    let mx = 80 - kv.length
     in kv <> renderComment mx c


renderKeywordValue :: Keyword -> Value -> BuilderBlock
renderKeywordValue k v =
  mconcat
    [ renderKeyword k
    , string "= "
    , pad 30 $ renderValue v
    ]


renderKeyword :: Keyword -> BuilderBlock
renderKeyword (Keyword k) = pad 8 $ string $ map toUpper $ take 8 $ unpack k


renderComment :: Int -> Comment -> BuilderBlock
renderComment mx (Comment c) = string $ take mx $ " / " <> unpack c


renderValue :: Value -> BuilderBlock
renderValue (Logic T) = justify 30 "T"
renderValue (Logic F) = justify 30 "F"
renderValue (Float f) = justify 30 $ string $ map toUpper $ show f
renderValue (Integer n) = justify 30 $ string $ show n
renderValue (String s) = string $ "'" <> unpack s <> "'"


-- Builder Block ---------------------------------------------------------

-- | We need a builder that keeps track of its length so we can pad things
data BuilderBlock = BuilderBlock {length :: Int, builder :: Builder}


instance IsString BuilderBlock where
  fromString = string


instance Semigroup BuilderBlock where
  BuilderBlock l b <> BuilderBlock l2 b2 = BuilderBlock (l + l2) (b <> b2)


instance Monoid BuilderBlock where
  mempty = BuilderBlock 0 mempty


justify :: Int -> BuilderBlock -> BuilderBlock
justify n b = spaces (n - b.length) <> b


pad :: Int -> BuilderBlock -> BuilderBlock
pad n b = b <> spaces (n - b.length)


spaces :: Int -> BuilderBlock
spaces n = BuilderBlock n $ mconcat $ replicate n $ charUtf8 ' '


string :: String -> BuilderBlock
string s = BuilderBlock (length s) (stringUtf8 s)
