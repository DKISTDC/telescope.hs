{-# OPTIONS_GHC -fno-warn-orphans #-}

module Telescope.Fits.Encoding.Render where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char (toUpper)
import Data.List qualified as L
import Data.String (IsString (..))
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text qualified as T
import Telescope.Data.Axes
import Telescope.Data.Numeric (showFloat)
import Telescope.Fits.BitPix
import Telescope.Fits.Checksum
import Telescope.Fits.DataArray
import Telescope.Fits.HDU.Block (hduBlockSize)
import Telescope.Fits.Header


renderDataArray :: ByteString -> ByteString
renderDataArray dat = BS.toStrict $ runRender $ renderData dat


renderData :: ByteString -> BuilderBlock
renderData s = fillBlock zeros $ BuilderBlock (BS.length s) $ byteString s


renderImageHeader :: Header -> DataArray -> Checksum -> BuilderBlock
renderImageHeader h d dsum =
  fillBlock spaces $
    mconcat
      [ renderKeywordLine "XTENSION" (String "IMAGE   ") (Just "Image Extension")
      , renderDataKeywords d.bitpix d.axes
      , renderKeywordLine "PCOUNT" (Integer 0) Nothing
      , renderKeywordLine "GCOUNT" (Integer 1) Nothing
      , renderDatasum dsum
      , renderRecords $ nonSystemRecords h
      , renderEnd
      ]


renderPrimaryHeader :: Header -> DataArray -> Checksum -> BuilderBlock
renderPrimaryHeader h d dsum =
  fillBlock spaces $
    mconcat
      [ renderKeywordLine "SIMPLE" (Logic T) (Just "Conforms to the FITS standard")
      , renderDataKeywords d.bitpix d.axes
      , renderKeywordLine "EXTEND" (Logic T) Nothing
      , renderDatasum dsum
      , renderRecords $ nonSystemRecords h
      , renderEnd
      ]


renderDatasum :: Checksum -> BuilderBlock
renderDatasum dsum =
  mconcat
    [ renderKeywordLine "DATASUM" (checksumValue dsum) Nothing
    , -- encode the CHECKSUM as zeros, replace later in 'runRenderHDU'
      renderKeywordLine "CHECKSUM" (String (T.replicate 16 "0")) Nothing
    ]


renderEnd :: BuilderBlock
renderEnd = pad 80 "END"


-- | Render required keywords for a data array
renderDataKeywords :: BitPix -> Axes Column -> BuilderBlock
renderDataKeywords bp (Axes as) =
  mconcat
    [ bitpix
    , naxis_
    , naxes
    ]
 where
  bitpix = renderKeywordLine "BITPIX" (Integer $ bitPixCode bp) (Just $ "(" <> bitPixType bp <> ") array data type")
  naxis_ = renderKeywordLine "NAXIS" (Integer $ length as) (Just "number of axes in data array")
  naxes = mconcat $ zipWith @Int naxisN [1 ..] as
  naxisN n a =
    let nt = pack (show n)
     in renderKeywordLine ("NAXIS" <> nt) (Integer a) (Just $ "axis " <> nt <> " length")
  bitPixType = pack . drop 2 . show
  bitPixCode :: BitPix -> Int
  bitPixCode = \case
    BPInt8 -> 8
    BPInt16 -> 16
    BPInt32 -> 32
    BPInt64 -> 64
    BPFloat -> -32
    BPDouble -> -64


renderRecords :: [HeaderRecord] -> BuilderBlock
renderRecords hrs =
  mconcat $ fmap renderHeaderRecord hrs


-- | 'Header' contains all other keywords. Filter out any that match system keywords so they aren't rendered twice
nonSystemRecords :: Header -> [HeaderRecord]
nonSystemRecords (Header ks) =
  filter (not . isSystemKeyword) ks
 where
  isSystemKeyword hr =
    case hr of
      Keyword kr ->
        let k = kr.keyword
         in k == "BITPIX"
              || k == "EXTEND"
              || k == "DATASUM"
              || k == "CHECKSUM"
              || k == "SIMPLE"
              || "NAXIS" `isPrefixOf` k
      _ -> False


renderHeaderRecord :: HeaderRecord -> BuilderBlock
renderHeaderRecord = \case
  Keyword kr -> renderKeywordLine kr.keyword kr.value kr.comment
  Comment c -> pad 80 $ string $ "COMMENT " <> unpack c
  History h -> pad 80 $ string $ "HISTORY " <> unpack h
  BlankLine -> pad 80 ""


-- | Fill out the header or data block to the nearest 2880 bytes
fillBlock :: (Int -> BuilderBlock) -> BuilderBlock -> BuilderBlock
fillBlock fill b =
  let rm = hduBlockSize - b.length `mod` hduBlockSize
   in b <> extraSpaces rm
 where
  extraSpaces n
    | n == hduBlockSize = mempty
    | otherwise = fill n


-- Keyword Lines -----------------------------------------------------

renderKeywordLine :: Text -> Value -> Maybe Text -> BuilderBlock
renderKeywordLine k v mc =
  let kv = renderKeywordValue k v
   in pad 80 $ insertComment kv mc
 where
  insertComment kv Nothing = kv
  insertComment kv (Just c) =
    let mx = 80 - kv.length
     in kv <> renderComment mx c


renderKeywordValue :: Text -> Value -> BuilderBlock
renderKeywordValue k v =
  mconcat
    [ renderKeyword k
    , string "= "
    , pad 20 $ renderValue v
    ]


renderKeyword :: Text -> BuilderBlock
renderKeyword k = pad 8 $ string $ map toUpper $ take 8 $ unpack k


renderComment :: Int -> Text -> BuilderBlock
renderComment mx c = string $ take mx $ " / " <> unpack c


renderValue :: Value -> BuilderBlock
renderValue (Logic T) = justify 20 "T"
renderValue (Logic F) = justify 20 "F"
renderValue (Float f) = justify 20 $ string $ showFloat f
renderValue (Integer n) = justify 20 $ string $ show n
renderValue (String s) = string $ "'" <> unpack s <> "'"


-- Builder Block ---------------------------------------------------------

-- | A builder that keeps track of its length so we can pad and justify things
data BuilderBlock = BuilderBlock {length :: Int, builder :: Builder}


-- | Smart constructor, don't allow negative lengths
builderBlock :: Int -> Builder -> BuilderBlock
builderBlock n = BuilderBlock (max n 0)


-- | Execute a BuilderBlock and create a bytestring
runRender :: BuilderBlock -> BL.ByteString
runRender bb = toLazyByteString bb.builder


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
spaces = padding (charUtf8 ' ')


zeros :: Int -> BuilderBlock
zeros = padding (word8 0)


padding :: Builder -> Int -> BuilderBlock
padding b n = builderBlock n . mconcat . replicate n $ b


string :: String -> BuilderBlock
string s = builderBlock (length s) (stringUtf8 s)


instance Show Header where
  show h =
    unlines $ L.unfoldr chunk $ BL8.unpack $ runRender $ renderRecords h.records
   where
    chunk :: String -> Maybe (String, String)
    chunk "" = Nothing
    chunk inp =
      Just $ splitAt 80 inp
