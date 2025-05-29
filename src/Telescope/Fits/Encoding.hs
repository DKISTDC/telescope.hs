module Telescope.Fits.Encoding where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Telescope.Data.Parser
import Telescope.Fits.Checksum
import Telescope.Fits.DataArray (DataArray (..), Dimensions (..), dataArray, dataSizeBytes)
import Telescope.Fits.Encoding.MegaHeader qualified as MH
import Telescope.Fits.Encoding.Render
import Telescope.Fits.HDU
import Telescope.Fits.Header
import Text.Megaparsec (lookAhead)
import Text.Megaparsec qualified as MP


{- | Decode a FITS file read from a strict 'ByteString'

>  decode =<< BS.readFile "samples/simple2x3.fits"
-}
decode :: forall m. (MonadThrow m) => ByteString -> m Fits
decode = fitsParseThrow parseFits


{- | Encode a FITS file to a strict 'ByteString'

> BS.writeFile $ encode fits
-}
encode :: Fits -> ByteString
encode f =
  let prim = encodePrimaryHDU f.primaryHDU
      exts = fmap encodeExtension f.extensions
   in mconcat $ prim : exts


fitsParseThrow :: (MonadThrow m) => Eff '[State ByteString, Parser] a -> ByteString -> m a
fitsParseThrow parse inp =
  case runFitsParse parse inp of
    Left e -> throwM e
    Right a -> pure a


runFitsParse :: Eff '[State ByteString, Parser] a -> BS.ByteString -> Either ParseError a
runFitsParse parse inp = do
  runPureEff $ runParser $ evalState inp parse


parseFits :: forall es. (State ByteString :> es, Parser :> es) => Eff es Fits
parseFits = do
  p <- primary
  es <- parseAt (Child "extensions") $ extensions 1
  pure $ Fits p es


primary :: (State ByteString :> es, Parser :> es) => Eff es DataHDU
primary = do
  parseAt (Child "primary") $ do
    (dm, hd) <- runMega "Primary Header" $ do
      dm <- MP.lookAhead MH.parsePrimaryKeywords
      hd <- MH.parseHeader
      pure (dm, hd)
    darr <- mainData dm
    pure $ DataHDU hd darr


extensions :: (State ByteString :> es, Parser :> es) => Int -> Eff es [Extension]
extensions n = do
  inp <- get @ByteString
  -- recurse until you run out of input or error
  case inp of
    "" -> pure []
    _ -> do
      e <- parseAt (Index n) extension
      es <- extensions (n + 1)
      pure (e : es)


extension :: (State ByteString :> es, Parser :> es) => Eff es Extension
extension = do
  resImg <- runParser image
  resTbl <- runParser binTable
  case (resImg, resTbl) of
    (Right i, _) -> pure $ Image i
    (_, Right b) -> pure $ BinTable b
    -- (Left (_, FormatError ie), Left (_, FormatError be)) -> throwM $ InvalidHDU [ie, be]
    (Left _, Left (ParseFailure p e)) -> do
      send $ PathMod (<> p) $ parseFail e


image :: (State ByteString :> es, Parser :> es) => Eff es DataHDU
image = do
  (dm, hd) <- runMega "Image Header" $ do
    dm <- MP.lookAhead MH.parseImageKeywords
    hd <- MH.parseHeader
    pure (dm, hd)
  darr <- mainData dm
  pure $ DataHDU hd darr


binTable :: (State ByteString :> es, Parser :> es) => Eff es BinTableHDU
binTable = do
  (dm, pcount, hd) <- do
    runMega "BinTable Header" $ do
      (dm, pcount) <- lookAhead MH.parseBinTableKeywords
      hd <- MH.parseHeader
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


-- | Parse HDUs by running MegaParsec parsers one at a time, and tracking how much of the ByteString we've consumed
runMega :: (Parser :> es, State ByteString :> es) => String -> MH.Parser a -> Eff es a
runMega src parse = do
  inp <- get
  case MH.runNextParser src inp parse of
    Right (a, rest) -> do
      put rest
      pure a
    Left err ->
      parseFail $ MH.showParseError err
