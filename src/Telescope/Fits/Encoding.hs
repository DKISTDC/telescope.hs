module Telescope.Fits.Encoding where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (toUpper)
import Data.Fits qualified as Fits
import Data.Fits.MegaParser (Parser)
import Data.Fits.MegaParser qualified as Fits
import Data.Fits.Read (FitsError (..))
import Data.String (IsString (..))
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Error.Static
import Effectful.NonDet
import Effectful.State.Static.Local
import Telescope.Fits.Checksum
import Telescope.Fits.Types
import Text.Megaparsec qualified as M
import Text.Megaparsec.State qualified as M


{- | Decode a FITS file read as a strict 'ByteString'

>  decode =<< BS.readFile "samples/simple2x3.fits"
-}
decode :: forall m. (MonadThrow m) => ByteString -> m Fits
decode inp = do
  let res = runPureEff $ evalState inp parseFits
  pure res


dataArray :: Fits.Dimensions -> ByteString -> DataArray
dataArray dim dat =
  DataArray
    { bitpix = bitpix dim._bitpix
    , axes = axes dim._axes
    , rawData = dat
    }
 where
  bitpix :: Fits.BitPixFormat -> BitPix
  bitpix Fits.EightBitInt = BPInt8
  bitpix Fits.SixteenBitInt = BPInt16
  bitpix Fits.ThirtyTwoBitInt = BPInt32
  bitpix Fits.SixtyFourBitInt = BPInt64
  bitpix Fits.ThirtyTwoBitFloat = BPFloat
  bitpix Fits.SixtyFourBitFloat = BPDouble

  axes :: Fits.Axes -> Axes Column
  axes = Axes


primary :: (State ByteString :> es) => Eff es PrimaryHDU
primary = do
  (dm, hd) <- nextParser "Primary Header" $ do
    dm <- Fits.parsePrimaryKeywords
    hd <- Fits.parseHeader
    pure (dm, hd)

  darr <- mainData dm
  pure $ PrimaryHDU hd darr


parseFits :: (State ByteString :> es) => Eff es Fits
parseFits = do
  p <- primary
  es <- extensions
  pure $ Fits p es
 where
  image :: (State ByteString :> es) => Eff es ImageHDU
  image = do
    (dm, hd) <- imageHeader
    darr <- mainData dm
    pure $ ImageHDU hd darr

  imageHeader :: (State ByteString :> es) => Eff es (Fits.Dimensions, Header)
  imageHeader = do
    nextParser "Image Header" $ do
      dm <- Fits.parseImageKeywords
      hd <- Fits.parseHeader
      pure (dm, hd)

  extension :: (State ByteString :> es) => Eff es Extension
  extension = do
    res <- runNonDet OnEmptyKeep $ do
      (Image <$> image) <|> (BinTable <$> binTable)
    case res of
      Left _ -> throwM $ InvalidExtension ""
      Right e -> pure e

  extensions :: (State ByteString :> es) => Eff es [Extension]
  extensions = do
    e <- extension
    rest <- get @ByteString
    case rest of
      "" -> pure [e]
      _ -> do
        es <- extensions
        pure (e : es)

  binTable :: (State ByteString :> es) => Eff es BinTableHDU
  binTable = do
    (dm, pcount, hd) <- binTableHeader
    darr <- mainData dm
    pure $ BinTableHDU hd pcount "" darr

  binTableHeader :: (State ByteString :> es) => Eff es (Fits.Dimensions, Int, Header)
  binTableHeader = do
    nextParser "Image Header" $ do
      (dm, pcount) <- Fits.parseBinTableKeywords
      hd <- Fits.parseHeader
      pure (dm, pcount, hd)


mainData :: (State ByteString :> es) => Fits.Dimensions -> Eff es DataArray
mainData dm = do
  rest <- get
  let len = Fits.dataSize dm
  let dat = dataArray dm (BS.take len rest)
  put $ BS.dropWhile (== 0) $ BS.drop len rest
  pure dat


-- Runs a parser, and returns the unconsumed string
nextParser :: (State ByteString :> es) => String -> Parser a -> Eff es a
nextParser src parse = do
  res <- runErrorNoCallStack @HDUError (nextParser' src parse)
  case res of
    Right a -> pure a
    Left e -> throwM e


nextParser' :: (Error HDUError :> es, State ByteString :> es) => String -> Parser a -> Eff es a
nextParser' src parse = do
  bs <- get
  let st1 = M.initialState src bs
  case M.runParser' parse st1 of
    (st2, Right a) -> do
      put $ BS.drop st2.stateOffset bs
      pure a
    (_, Left err) -> throwError $ FormatError $ ParseError err


data HDUError
  = InvalidExtension String
  | MissingPrimary
  | FormatError FitsError
  deriving (Show, Exception)


{- | Encode a FITS file to a strict 'ByteString'

> BS.writeFile $ encdoe fits
-}
encode :: Fits -> ByteString
encode f =
  let primary = encodePrimaryHDU f.primaryHDU
      exts = fmap encodeExtension f.extensions
   in mconcat $ primary : exts


-- | Execute a BuilderBlock and create a bytestring
runRender :: BuilderBlock -> BL.ByteString
runRender bb = toLazyByteString bb.builder


encodePrimaryHDU :: PrimaryHDU -> ByteString
encodePrimaryHDU p = encodeHDU (renderPrimaryHeader p.header p.dataArray) p.dataArray.rawData


encodeImageHDU :: ImageHDU -> ByteString
encodeImageHDU p = encodeHDU (renderImageHeader p.header p.dataArray) p.dataArray.rawData


encodeExtension :: Extension -> ByteString
encodeExtension (Image hdu) = encodeImageHDU hdu
encodeExtension (BinTable _) = error "BinTableHDU rendering not supported"


encodeHDU :: (Checksum -> BuilderBlock) -> ByteString -> ByteString
encodeHDU buildHead rawData =
  let dsum = checksum rawData
   in encodeHeader (buildHead dsum) dsum <> encodeDataArray rawData


encodeHeader :: BuilderBlock -> Checksum -> ByteString
encodeHeader buildHead dsum =
  let h = BS.toStrict $ runRender buildHead
      hsum = checksum h -- calculate the checksum of only the header
      csum = hsum <> dsum -- 1s complement add to the datasum
   in replaceChecksum csum h


encodeDataArray :: ByteString -> ByteString
encodeDataArray dat = BS.toStrict $ runRender $ renderData dat


replaceChecksum :: Checksum -> ByteString -> ByteString
replaceChecksum csum = replaceKeywordLine "CHECKSUM" (String $ encodeChecksum csum)


replaceKeywordLine :: ByteString -> Value -> ByteString -> ByteString
replaceKeywordLine key val header =
  let (start, rest) = BS.breakSubstring key header
      newKeyLine = BS.toStrict $ runRender $ renderKeywordLine (TE.decodeUtf8 key) val Nothing
   in start <> newKeyLine <> BS.drop 80 rest


-- renderPrimaryHDU :: PrimaryHDU -> BuilderBlock
-- renderPrimaryHDU hdu =
--   let dsum = checksum hdu.dataArray.rawData
--    in mconcat
--         [ renderPrimaryHeader hdu.header hdu.dataArray dsum
--         , renderData hdu.dataArray.rawData
--         ]

-- renderExtensionHDU :: Extension -> BuilderBlock
-- renderExtensionHDU (Image hdu) = renderImageHDU hdu
-- renderExtensionHDU (BinTable _) = error "BinTableHDU rendering not supported"

-- renderImageHDU :: ImageHDU -> BuilderBlock
-- renderImageHDU hdu =
--   let dsum = checksum hdu.dataArray.rawData
--    in mconcat
--         [ renderImageHeader hdu.header hdu.dataArray dsum
--         , renderData hdu.dataArray.rawData
--         ]

renderData :: ByteString -> BuilderBlock
renderData s = fillBlock zeros $ BuilderBlock (BS.length s) $ byteString s


renderImageHeader :: Header -> DataArray -> Checksum -> BuilderBlock
renderImageHeader h d dsum =
  fillBlock spaces $
    mconcat
      [ renderKeywordLine "XTENSION" (String "IMAGE") (Just "Image Extension")
      , renderDataKeywords d.bitpix d.axes
      , renderKeywordLine "PCOUNT" (Integer 0) Nothing
      , renderKeywordLine "GCOUNT" (Integer 1) Nothing
      , renderDatasum dsum
      , renderOtherKeywords h
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
      , renderOtherKeywords h
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


-- | 'Header' contains all other keywords. Filter out any that match system keywords so they aren't rendered twice
renderOtherKeywords :: Header -> BuilderBlock
renderOtherKeywords (Header ks) =
  mconcat $ map toLine $ filter (not . isSystemKeyword) ks
 where
  toLine (Keyword kr) = renderKeywordLine kr._keyword kr._value kr._comment
  toLine (Comment c) = pad 80 $ string $ "COMMENT " <> unpack c
  toLine BlankLine = pad 80 ""
  isSystemKeyword (Keyword kr) =
    let k = kr._keyword
     in k == "BITPIX"
          || k == "EXTEND"
          || k == "DATASUM"
          || k == "CHECKSUM"
          || "NAXIS" `isPrefixOf` k
  isSystemKeyword _ = False


-- | Fill out the header or data block to the nearest 2880 bytes
fillBlock :: (Int -> BuilderBlock) -> BuilderBlock -> BuilderBlock
fillBlock fill b =
  let rm = hduBlockSize - b.length `mod` hduBlockSize
   in b <> extraSpaces rm
 where
  extraSpaces n
    | n == hduBlockSize = mempty
    | otherwise = fill n


bitPixCode :: BitPix -> Int
bitPixCode BPInt8 = 8
bitPixCode BPInt16 = 16
bitPixCode BPInt32 = 32
bitPixCode BPInt64 = 64
bitPixCode BPFloat = -32
bitPixCode BPDouble = -64


-- Keyword Lines -----------------------------------------------------

renderKeywordLine :: Text -> Value -> Maybe Text -> BuilderBlock
renderKeywordLine k v mc =
  let kv = renderKeywordValue k v
   in pad 80 $ addComment kv mc
 where
  addComment kv Nothing = kv
  addComment kv (Just c) =
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
renderValue (Float f) = justify 20 $ string $ map toUpper $ show f
renderValue (Integer n) = justify 20 $ string $ show n
renderValue (String s) = string $ "'" <> unpack s <> "'"


-- Builder Block ---------------------------------------------------------

-- | We need a builder that keeps track of its length so we can pad things
data BuilderBlock = BuilderBlock {length :: Int, builder :: Builder}


-- | Smart constructor, don't allow negative lengths
builderBlock :: Int -> Builder -> BuilderBlock
builderBlock n = BuilderBlock (max n 0)


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
