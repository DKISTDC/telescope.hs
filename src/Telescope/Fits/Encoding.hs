module Telescope.Fits.Encoding where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Telescope.Fits.Checksum
import Telescope.Fits.DataArray (DataArray (..), Dimensions (..), dataArray, dataSizeBytes)
import Telescope.Fits.Encoding.MegaParser (Parser, ParseErr)
import Telescope.Fits.Encoding.MegaParser qualified as P
-- import Telescope.Fits.Encoding.Read
import Telescope.Fits.Encoding.Render
import Telescope.Fits.HDU
import Telescope.Fits.Header
import Text.Megaparsec qualified as MP
import Text.Megaparsec (lookAhead)


{- | Decode a FITS file read as a strict 'ByteString'

>  decode =<< BS.readFile "samples/simple2x3.fits"
-}
decode :: forall m. (MonadThrow m) => ByteString -> m Fits
decode = parseThrow parseFits


{- | Encode a FITS file to a strict 'ByteString'

> BS.writeFile $ encdoe fits
-}
encode :: Fits -> ByteString
encode f =
  let prim = encodePrimaryHDU f.primaryHDU
      exts = fmap encodeExtension f.extensions
   in mconcat $ prim : exts

parseThrow :: MonadThrow m => Eff '[State ByteString, Error HDUError] a -> ByteString -> m a
parseThrow parse inp =
  case runParseBytes inp parse of
    Left e -> throwM e
    Right a -> pure a


-- | Parses a stream of bytes from the file 
runParseBytes :: BS.ByteString -> Eff '[State ByteString, Error HDUError] a -> Either HDUError a
runParseBytes inp parse = do
  runPureEff $ runErrorNoCallStack @HDUError $ evalState inp parse


encodePrimaryHDU :: DataHDU -> ByteString
encodePrimaryHDU p =
  encodeHDU (renderPrimaryHeader p.header p.dataArray) p.dataArray.rawData


encodeImageHDU :: DataHDU -> ByteString
encodeImageHDU p =
  encodeHDU (renderImageHeader p.header p.dataArray) p.dataArray.rawData


encodeExtension :: Extension -> ByteString
encodeExtension (Image hdu) = encodeImageHDU hdu
encodeExtension (BinTable _) = error "BinTableHDU rendering not supported"


-- | Encode an HDU, properly handling datasum and checksum
encodeHDU :: (Checksum -> BuilderBlock) -> ByteString -> ByteString
encodeHDU buildHead rawData =
  let dsum = checksum rawData
   in encodeHeader dsum <> renderDataArray rawData
 where
  encodeHeader :: Checksum -> ByteString
  encodeHeader dsum =
    let h = BS.toStrict $ runRender (buildHead dsum)
        hsum = checksum h -- calculate the checksum of only the header
        csum = hsum <> dsum -- 1s complement add to the datasum
     in replaceChecksum csum h

  replaceChecksum :: Checksum -> ByteString -> ByteString
  replaceChecksum csum = replaceKeywordLine "CHECKSUM" (String $ encodeChecksum csum) Nothing


-- | Fast replace a single keyword in a raw header bytestring
replaceKeywordLine :: ByteString -> Value -> Maybe Text -> ByteString -> ByteString
replaceKeywordLine key val mc header =
  let (start, rest) = BS.breakSubstring key header
      newKeyLine = BS.toStrict $ runRender $ renderKeywordLine (TE.decodeUtf8 key) val mc
   in start <> newKeyLine <> BS.drop 80 rest


parseFits :: forall es. (State ByteString :> es, Error HDUError :> es) => Eff es Fits
parseFits = do
  p <- primary
  es <- extensions
  pure $ Fits p es


primary :: (State ByteString :> es, Error HDUError :> es) => Eff es DataHDU
primary = do
  (dm, hd) <- runMega "Primary Header" $ do
    dm <- MP.lookAhead P.parsePrimaryKeywords
    hd <- P.parseHeader
    pure (dm, hd)
  darr <- mainData dm
  pure $ DataHDU hd darr

extensions :: (State ByteString :> es, Error HDUError :> es) => Eff es [Extension]
extensions = do
  inp <- get @ByteString
  case inp of
    "" -> pure []
    _ -> do
      e <- extension
      es <- extensions
      pure (e : es)

extension :: (State ByteString :> es, Error HDUError :> es) => Eff es Extension
extension = do
  -- this consumes input!
  resImg <- tryError @HDUError image
  resTbl <- tryError @HDUError binTable
  case (resImg, resTbl) of
    (Right i, _) -> pure $ Image i
    (_, Right b) -> pure $ BinTable b
    -- (Left (_, FormatError ie), Left (_, FormatError be)) -> throwM $ InvalidHDU [ie, be]
    (Left _, Left (_, be)) -> throwM be

image :: (State ByteString :> es, Error HDUError :> es) => Eff es DataHDU
image = do
  (dm, hd) <- runMega "Image Header" $ do
    dm <- MP.lookAhead P.parseImageKeywords
    hd <- P.parseHeader
    pure (dm, hd)
  darr <- mainData dm
  pure $ DataHDU hd darr

binTable :: (State ByteString :> es, Error HDUError :> es) => Eff es BinTableHDU
binTable = do
  (dm, pcount, hd) <-  do
    runMega "BinTable Header" $ do
      (dm, pcount) <- lookAhead P.parseBinTableKeywords
      hd <- P.parseHeader
      pure (dm, pcount, hd)

  darr <- mainData dm
  rest <- get
  let heap = BS.take pcount rest
  put $ BS.dropWhile (== 0) $ BS.drop pcount rest
  pure $ BinTableHDU hd pcount heap darr


mainData :: (State ByteString :> es) => Dimensions -> Eff es DataArray
mainData dm = do
  rest <- get
  let len = dataSizeBytes dm
  let dat = dataArray dm (BS.take len rest)
  put $ BS.dropWhile (== 0) $ BS.drop len rest
  pure dat

-- | Parse HDUs by running MegaParsec parsers one at a time, tracking how much of the ByteString we've consumed
runMega :: (Error HDUError :> es, State ByteString :> es) => String -> Parser a -> Eff es a
runMega src parse = do
  inp <- get
  case P.runNextParser src inp parse of
    Right (a, rest) -> do
      put rest
      pure a
    Left err ->
      throwError $ ParseError err


data HDUError
  = InvalidExtension String
  | MissingPrimary
  | ParseError ParseErr
  -- | FormatError FitsError
  -- | InvalidHDU [FitsError]
  deriving (Exception)

instance Show HDUError where
  show (InvalidExtension s) = "InvalidExtension: " <> s
  show MissingPrimary = "Missing Primary HDU"
  show (ParseError e) = "ParseError " <> MP.errorBundlePretty e

