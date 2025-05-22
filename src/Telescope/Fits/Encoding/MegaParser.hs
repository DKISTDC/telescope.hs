{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Module      : Telescope.Fits.Encoding.MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2023
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}
module Telescope.Fits.Encoding.MegaParser where

import Control.Monad (replicateM_, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Data.Word (Word8)
import Telescope.Data.Axes
import Telescope.Fits.BitPix
import Telescope.Fits.DataArray
import Telescope.Fits.HDU
import Telescope.Fits.HDU.Block (hduRecordLength)
import Telescope.Fits.Header hiding (FromKeyword (..), parseHeader, parseKeyword)
import Text.Megaparsec (ParseErrorBundle, Parsec, (<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte qualified as M
import Text.Megaparsec.Byte.Lexer qualified as MBL
import Text.Megaparsec.Pos qualified as MP
import Text.Megaparsec.State qualified as M


type Parser = Parsec Void ByteString
type ParseErr = ParseErrorBundle ByteString Void


runNextParser :: String -> BS.ByteString -> Parser a -> Either ParseErr (a, BS.ByteString)
runNextParser src inp parse = do
  let st1 = M.initialState src inp
  case M.runParser' parse st1 of
    (st2, Right a) -> do
      -- only consumes input if it succeeds
      let rest = BS.drop st2.stateOffset inp
      pure (a, rest)
    (_, Left err) -> Left err


toWord :: Char -> Word8
toWord = fromIntegral . ord


wordsText :: [Word8] -> Text
wordsText = TE.decodeUtf8 . BS.pack


-- | Consumes ALL header blocks until end, then all remaining space
parseHeader :: Parser Header
parseHeader = do
  pairs <- M.manyTill parseRecordLine (M.string' "end")
  M.space -- consume space padding all the way to the end of the next 2880 bytes header block
  return $ Header pairs


parseRecordLine :: Parser HeaderRecord
parseRecordLine = do
  M.try (Keyword <$> parseKeywordRecord)
    <|> M.try (Comment <$> parseLineComment)
    <|> BlankLine
    <$ parseLineBlank


parseKeywordRecord :: Parser KeywordRecord
parseKeywordRecord = do
  ((k, v), mc) <- withComments parseKeywordValue
  pure $ KeywordRecord k v mc


-- | Parses the specified keyword
parseKeywordRecord' :: ByteString -> Parser a -> Parser a
parseKeywordRecord' k pval = ignoreComments $ do
  _ <- M.string' k
  parseEquals
  pval


-- | Combinator to allow for parsing a record with inline comments
withComments :: Parser a -> Parser (a, Maybe Text)
withComments parse = do
  -- assumes we are at the beginning of the line
  lineStart <- parsePos
  a <- parse
  mc <- parseLineEnd lineStart
  return (a, mc)


ignoreComments :: Parser a -> Parser a
ignoreComments parse = do
  (a, _) <- withComments parse
  pure a


parseKeywordValue :: Parser (Text, Value)
parseKeywordValue = do
  key <- parseKeyword
  parseEquals
  val <- parseValue
  return (key, val)


parseLineEnd :: Int -> Parser (Maybe Text)
parseLineEnd lineStart = do
  M.try (Nothing <$ spacesToLineEnd lineStart) <|> (Just <$> parseInlineComment lineStart)


spacesToLineEnd :: Int -> Parser ()
spacesToLineEnd lineStart = do
  curr <- parsePos
  let used = curr - lineStart
  parseSpacesN (hduRecordLength - used)
  pure ()


parseSpacesN :: Int -> Parser ()
parseSpacesN n = replicateM_ n (M.char $ toWord ' ')


parseInlineComment :: Int -> Parser Text
parseInlineComment lineStart = do
  -- any number of spaces... the previous combinator has eaten up blank lines already
  M.space
  _ <- M.char $ toWord '/'
  _ <- M.optional charSpace
  curr <- parsePos
  let used = curr - lineStart
  c <- M.count (hduRecordLength - used) M.anySingle
  return $ T.strip $ wordsText c
 where
  charSpace = M.char $ toWord ' '


parseLineComment :: Parser Text
parseLineComment = do
  let kw = "COMMENT " :: ByteString
  _ <- M.string' kw
  c <- M.count (hduRecordLength - BS.length kw) M.anySingle
  return $ wordsText c


parseLineBlank :: Parser ()
parseLineBlank = do
  _ <- M.string' (BS.replicate hduRecordLength (toWord ' '))
  pure ()


-- | Anything but a space or equals
parseKeyword :: Parser Text
parseKeyword = wordsText <$> M.some (M.noneOf $ fmap toWord [' ', '='])


parseValue :: Parser Value
parseValue =
  -- try is required here because Megaparsec doesn't automatically backtrack if the parser consumes anything
  M.try (Float <$> parseFloat)
    <|> M.try (Integer <$> parseInt)
    <|> (Logic <$> parseLogic)
    <|> (String <$> parseStringContinue)


parseInt :: (Num a) => Parser a
parseInt = MBL.signed M.space MBL.decimal


parseFloat :: Parser Double
parseFloat = MBL.signed M.space MBL.float


parseLogic :: Parser LogicalConstant
parseLogic = do
  T <$ M.string' "T" <|> F <$ M.string' "F"


parseStringContinue :: Parser Text
parseStringContinue = do
  t <- parseStringValue

  mc <- M.optional $ do
    _ <- M.string' "CONTINUE"
    M.space
    parseStringContinue

  case mc of
    Nothing -> return t
    Just tc -> return $ T.dropWhileEnd (== '&') t <> tc


parseStringValue :: Parser Text
parseStringValue = do
  -- The rules are weird, NULL means a NULL string, '' is an empty
  -- string, a ' followed by a bunch of spaces and a close ' is
  -- considered an empty string, and trailing whitespace is ignored
  -- within the quotes, but not leading spaces.
  ls <- M.between (M.char quote) (M.char quote) $ M.many $ M.anySingleBut quote
  consumeDead
  return (T.stripEnd $ wordsText ls)
 where
  quote = toWord '\''


skipEmpty :: Parser ()
skipEmpty = void (M.many $ M.satisfy (toWord '\0' ==))


consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty


parseEnd :: Parser ()
parseEnd = M.string' "end" >> M.space <* M.eof


parseEquals :: Parser ()
parseEquals = M.space >> M.char (toWord '=') >> M.space


parsePos :: Parser Int
parsePos = MP.unPos . MP.sourceColumn <$> M.getSourcePos


parseBitPix :: Parser BitPix
parseBitPix = do
  v <- parseKeywordRecord' "BITPIX" parseValue
  toBitpix v
 where
  toBitpix (Integer 8) = return BPInt8
  toBitpix (Integer 16) = return BPInt16
  toBitpix (Integer 32) = return BPInt32
  toBitpix (Integer 64) = return BPInt64
  toBitpix (Integer (-32)) = return BPFloat
  toBitpix (Integer (-64)) = return BPDouble
  toBitpix _ = fail "Invalid BITPIX header"


parseNaxes :: Parser (Axes Column)
parseNaxes = do
  n <- parseKeywordRecord' "NAXIS" parseInt
  Axes <$> mapM parseN [1 .. n]
 where
  parseN :: Int -> Parser Int
  parseN n = parseKeywordRecord' (C8.pack $ "NAXIS" <> show n) parseInt


-- | We don't parse simple here, because it isn't required on all HDUs
parseDimensions :: Parser Dimensions
parseDimensions = do
  bp <- parseBitPix
  Dimensions bp <$> parseNaxes


parsePrimaryKeywords :: Parser Dimensions
parsePrimaryKeywords = do
  _ <- parseKeywordRecord' "SIMPLE" parseLogic
  parseDimensions


parseImageKeywords :: Parser Dimensions
parseImageKeywords = do
  _ <- ignoreComments $ M.string' "XTENSION= 'IMAGE   '"
  parseDimensions


parseBinTableKeywords :: Parser (Dimensions, Int)
parseBinTableKeywords = do
  _ <- ignoreComments $ M.string' "XTENSION= 'BINTABLE'"
  sz <- parseDimensions
  pc <- parseKeywordRecord' "PCOUNT" parseInt
  return (sz, pc)

-- parsePrimary :: Parser DataHDU
-- parsePrimary = do
--   -- do not consume the headers used for the dimensions
--   dm <- M.lookAhead parsePrimaryKeywords
--   hd <- parseHeader
--   dt <- parseMainData dm
--   return $ DataHDU hd (dataArray dm dt)

-- parseImage :: Parser DataHDU
-- parseImage = do
--   -- do not consume the headers used for the dimensions
--   dm <- M.lookAhead parseImageKeywords
--   hd <- parseHeader
--   dt <- parseMainData dm
--   return $ DataHDU hd (dataArray dm dt)
-- parseBinTable :: Parser BinTableHDU
-- parseBinTable = do
--   (dm, pc) <- M.lookAhead parseBinTableKeywords
--   hd <- parseHeader
--   dt <- parseMainData dm
--   hp <- parseBinTableHeap
--   return $ BinTableHDU hd pc hp (dataArray dm dt)
--  where
--   parseBinTableHeap = return ""

-- parseMainData :: Dimensions -> Parser ByteString
-- parseMainData size = do
--   let len = dataSizeBytes size
--   M.takeP (Just ("Data Array of " <> show len <> " Bytes")) (fromIntegral len)
--
--
-- parseExtensions :: Parser [Extension]
-- parseExtensions = do
--   M.many parseExtension
--  where
--   parseExtension :: Parser Extension
--   parseExtension =
--     Image <$> parseImage <|> BinTable <$> parseBinTable
