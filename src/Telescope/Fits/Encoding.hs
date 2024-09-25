module Telescope.Fits.Encoding
  ( -- * Decoding
    decode

    -- * Encoding
  , encode
  , encodePrimaryHDU
  , encodeImageHDU
  , encodeExtension
  , encodeHDU
  , replaceKeywordLine

    -- * Parser
  , nextParser
  , nextParserThrow
  , parseFits
  , parseMainData
  , HDUError (..)
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fits qualified as Fits
import Data.Fits.MegaParser (Parser)
import Data.Fits.MegaParser qualified as Fits
import Data.Fits.Read (FitsError (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Telescope.Fits.Checksum
import Telescope.Fits.DataArray (dataArray)
import Telescope.Fits.Encoding.Render
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


{- | Encode a FITS file to a strict 'ByteString'

> BS.writeFile $ encdoe fits
-}
encode :: Fits -> ByteString
encode f =
  let primary = encodePrimaryHDU f.primaryHDU
      exts = fmap encodeExtension f.extensions
   in mconcat $ primary : exts


encodePrimaryHDU :: PrimaryHDU -> ByteString
encodePrimaryHDU p =
  encodeHDU (renderPrimaryHeader p.header p.dataArray) p.dataArray.rawData


encodeImageHDU :: ImageHDU -> ByteString
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


parseFits :: (State ByteString :> es) => Eff es Fits
parseFits = do
  p <- primary
  es <- extensions
  pure $ Fits p es
 where
  primary :: (State ByteString :> es) => Eff es PrimaryHDU
  primary = do
    (dm, hd) <- nextParserThrow "Primary Header" $ do
      dm <- Fits.parsePrimaryKeywords
      hd <- Fits.parseHeader
      pure (dm, hd)

    darr <- parseMainData dm
    pure $ PrimaryHDU hd darr

  image :: (Error HDUError :> es, State ByteString :> es) => Eff es ImageHDU
  image = do
    (dm, hd) <- imageHeader
    darr <- parseMainData dm
    pure $ ImageHDU hd darr

  imageHeader :: (Error HDUError :> es, State ByteString :> es) => Eff es (Fits.Dimensions, Header)
  imageHeader = do
    nextParser "Image Header" $ do
      dm <- Fits.parseImageKeywords
      hd <- Fits.parseHeader
      pure (dm, hd)

  extension :: (State ByteString :> es) => Eff es Extension
  extension = do
    -- this consumes input!
    resImg <- runErrorNoCallStack @HDUError image
    resTbl <- runErrorNoCallStack @HDUError binTable
    case (resImg, resTbl) of
      (Right i, _) -> pure $ Image i
      (_, Right b) -> pure $ BinTable b
      -- should report the current extenion
      (_, Left be) -> throwM be

  extensions :: (State ByteString :> es) => Eff es [Extension]
  extensions = do
    inp <- get @ByteString
    case inp of
      "" -> pure []
      _ -> do
        e <- extension
        es <- extensions
        pure (e : es)

  binTable :: (Error HDUError :> es, State ByteString :> es) => Eff es BinTableHDU
  binTable = do
    (dm, pcount, hd) <- binTableHeader
    darr <- parseMainData dm
    rest <- get
    let heap = BS.take pcount rest
    put $ BS.dropWhile (== 0) $ BS.drop pcount rest
    pure $ BinTableHDU hd pcount heap darr

  binTableHeader :: (Error HDUError :> es, State ByteString :> es) => Eff es (Fits.Dimensions, Int, Header)
  binTableHeader = do
    nextParser "BinTable Header" $ do
      (dm, pcount) <- Fits.parseBinTableKeywords
      hd <- Fits.parseHeader
      pure (dm, pcount, hd)


parseMainData :: (State ByteString :> es) => Fits.Dimensions -> Eff es DataArray
parseMainData dm = do
  rest <- get
  let len = Fits.dataSize dm
  let dat = dataArray dm (BS.take len rest)
  put $ BS.dropWhile (== 0) $ BS.drop len rest
  pure dat


-- | Parse HDUs by running MegaParsec parsers one at a time, tracking how much of the ByteString we've consumed
nextParser :: (Error HDUError :> es, State ByteString :> es) => String -> Parser a -> Eff es a
nextParser src parse = do
  bs <- get
  let st1 = M.initialState src bs
  case M.runParser' parse st1 of
    (st2, Right a) -> do
      -- only consumes input if it succeeds
      put $ BS.drop st2.stateOffset bs
      pure a
    (_, Left err) -> throwError $ FormatError $ ParseError err


nextParserThrow :: (State ByteString :> es) => String -> Parser a -> Eff es a
nextParserThrow src parse = do
  res <- runErrorNoCallStack @HDUError (nextParser src parse)
  case res of
    Right a -> pure a
    Left e -> throwM e


data HDUError
  = InvalidExtension String
  | MissingPrimary
  | FormatError FitsError
  deriving (Show, Exception)
