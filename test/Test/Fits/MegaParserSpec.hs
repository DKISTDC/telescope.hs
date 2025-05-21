{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fits.MegaParserSpec where

import Control.Exception (Exception (displayException))
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Fits
import Data.Fits qualified as Fits
import Data.Fits.MegaParser
import Data.Fits.Read
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Skeletest
import System.IO
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Prelude hiding (lookup)


spec :: Spec
spec = do
  basicParsing
  keywordValueLines
  comments
  continue
  fullRecord
  fullRecordLine
  headerMap
  requiredHeaders
  dataArray
  sampleNSOHeaders
  sampleNSO


parse :: Parser a -> ByteString -> IO a
parse p inp =
  case M.parse p "Test" inp of
    Left e -> fail $ displayException e
    Right v -> pure v


flattenKeywords :: [ByteString] -> ByteString
flattenKeywords ts = mconcat (map pad ts) <> "END"


pad :: ByteString -> ByteString
pad m =
  let n = 80 - BS.length m
   in m <> C8.replicate n ' '


basicParsing :: Spec
basicParsing = describe "Basic Parsing" $ do
  it "should parse a string" $ do
    res <- parse parseStringValue "'hello there' "
    res `shouldBe` "hello there"

  it "should parse a number value" $ do
    res <- parse parseValue "42 "
    res `shouldBe` Integer 42

  it "should parse a keyword" $ do
    res <- parse parseKeyword "WS_TEMP ="
    res `shouldBe` "WS_TEMP"

  it "should handle keyword symbols" $ do
    res <- parse parseKeyword "OBSGEO-X=   -5466045.256954942 / [m]"
    res `shouldBe` "OBSGEO-X"


keywordValueLines :: Spec
keywordValueLines = describe "parse keyword=value" $ do
  it "should parse an integer" $ do
    res <- parse parseKeywordValue "KEY=42 "
    res `shouldBe` ("KEY", Integer 42)

  it "should parse a string" $ do
    res <- parse parseKeywordValue "KEY='value'"
    res `shouldBe` ("KEY", String "value")

  it "should absorb spaces" $ do
    res <- parse parseKeywordValue "KEY   = 'value'   "
    res `shouldBe` ("KEY", String "value")

  it "should parse a float" $ do
    res <- parse parseKeywordValue "KEY =   44.88 "
    res `shouldBe` ("KEY", Float 44.88)

  it "should parse a negative number" $ do
    res <- parse parseKeywordValue "KEY = -44.88"
    res `shouldBe` ("KEY", Float (-44.88))

  it "should parse a logical constant" $ do
    res <- parse parseKeywordValue "KEYT=     T "
    res `shouldBe` ("KEYT", Logic T)

    res2 <- parse parseKeywordValue "KEYF=     F "
    res2 `shouldBe` ("KEYF", Logic F)

  it "should ignore comments" $ do
    res <- parse parseKeywordValue "SIMPLE  =                    T / conforms to FITS standard"
    res `shouldBe` ("SIMPLE", Logic T)

  it "should strip trailing spaces from strings" $ do
    res <- parse parseKeywordValue "INSTRUME= 'VISP    '"
    res `shouldBe` ("INSTRUME", String "VISP")


fullRecord :: Spec
fullRecord = describe "parseKeywordRecord" $ do
  it "should parse an 80 character record" $ do
    res <- parse parseKeywordRecord (flattenKeywords ["KEYWORD = 12345"])
    res `shouldBe` KeywordRecord "KEYWORD" (Integer 12345) Nothing

  it "should parse an a record and comment" $ do
    res <- parse parseKeywordRecord (flattenKeywords ["KEYWORD = 12345 / this is a comment"])
    res `shouldBe` KeywordRecord "KEYWORD" (Integer 12345) (Just "this is a comment")

  it "should parse a record, comment, followed by next keyword" $ do
    res <- parse parseKeywordRecord $ flattenKeywords ["SIMPLE  =                    T / conforms to FITS standard"]
    res `shouldBe` KeywordRecord "SIMPLE" (Logic T) (Just "conforms to FITS standard")

  it "should handle keyword symbols" $ do
    res <- parse parseKeywordRecord $ flattenKeywords ["OBSGEO-X=   -5466045.256954942 / [m]"]
    res `shouldBe` KeywordRecord "OBSGEO-X" (Float (-5466045.256954942)) (Just "[m]")

  it "should handle extension" $ do
    res <- parse parseKeywordRecord $ flattenKeywords ["XTENSION= 'IMAGE   '"]
    res `shouldBe` KeywordRecord "XTENSION" (String "IMAGE") Nothing

  it "should not consume blank lines" $ do
    let inp = flattenKeywords ["SIMPLE  =                    T", " "]
    res <- flip parse inp $ do
      kv <- parseKeywordRecord
      _ <- parseLineBlank
      pure kv
    res `shouldBe` KeywordRecord "SIMPLE" (Logic T) Nothing


fullRecordLine :: Spec
fullRecordLine = describe "parseRecordLine" $ do
  it "should parse a normal line" $ do
    res <- parse parseRecordLine "NAXIS1  =                  100 / [pix]                                          END"
    res `shouldBe` Keyword (KeywordRecord "NAXIS1" (Integer 100) (Just "[pix]"))

  it "should parse a comment line" $ do
    res <- parse parseRecordLine "COMMENT ------------------------------ Telescope -------------------------------END"
    res `shouldBe` Comment "------------------------------ Telescope -------------------------------"

  it "should parse a blank line" $ do
    res <- parse parseRecordLine $ flattenKeywords [" "]
    res `shouldBe` BlankLine


comments :: Spec
comments = do
  describe "Full-line comments" $ do
    it "should parse full-line comments" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT --------------------------- VISP Instrument ----------------------------"]
      res `shouldBe` "--------------------------- VISP Instrument ----------------------------"

    it "should parse comments with text" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT  Keys describing the pointing and operation of the telescope. Including "]
      res `shouldBe` " Keys describing the pointing and operation of the telescope. Including "

    it "should parse blank comments" $ do
      res <- parse parseLineComment $ flattenKeywords ["COMMENT                                                                         "]
      res `shouldBe` "                                                                        "

  describe "inline comments" $ do
    it "should parse comment" $ do
      res <- parse (parseInlineComment 0) $ " / Telescope" <> C8.replicate 68 ' '
      res `shouldBe` "Telescope"

  describe "parse to line end" $ do
    it "should parse comment line end" $ do
      res <- parse (parseLineEnd 0) $ " / Telescope" <> C8.replicate 68 ' '
      res `shouldBe` Just "Telescope"

    it "should ignore blanks" $ do
      res <- parse (parseLineEnd 0) $ C8.replicate 80 ' '
      res `shouldBe` Nothing

    it "should parse comment after blanks" $ do
      res <- parse (parseLineEnd 0) $ "          / comment " <> C8.replicate 60 ' '
      res `shouldBe` Just "comment"

  describe "withComments" $ do
    it "should parse a number, ignoring" $ do
      res <- parse (withComments parseValue) $ "12345 / Woot" <> C8.replicate 68 ' '
      res `shouldBe` (Integer 12345, Just "Woot")

    it "should parse no comment" $ do
      res <- parse (withComments parseValue) $ "12345       " <> C8.replicate 68 ' '
      res `shouldBe` (Integer 12345, Nothing)

    it "should ignore comments on bintable" $ do
      -- parse (withComments parseValue) $ "12345       " <> C8.replicate 68 ' '
      let inp = flattenKeywords ["XTENSION= 'BINTABLE'           / binary table extension"]
      (_, mc) <- flip parse inp $ do
        withComments $ M.string' "XTENSION= 'BINTABLE'"
      mc `shouldBe` Just "binary table extension"


continue :: Spec
continue = describe "Continue Keyword" $ do
  it "should be picked up in parseValue" $ do
    res <- parse parseValue $ flattenKeywords ["'hello&'CONTINUE '!'"]
    res `shouldBe` String "hello!"

  it "should combine continue into previous keyword" $ do
    let hs =
          [ "CAL_URL = 'https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.ht&'"
          , "CONTINUE  'ml'                                                                  "
          ]

    h <- parse parseHeader $ flattenKeywords hs
    Fits.lookup "CAL_URL" h `shouldBe` Just (String "https://docs.dkist.nso.edu/projects/visp/en/v2.0.1/l0_to_l1_visp.html")


headerMap :: Spec
headerMap = describe "full header" $ do
  it "should parse single header" $ do
    h <- parse parseHeader $ flattenKeywords ["KEY1='value'"]
    length (keywords h) `shouldBe` 1
    Fits.lookup "KEY1" h `shouldBe` Just (String "value")

  it "should parse multiple headers " $ do
    res <- parse parseHeader $ flattenKeywords ["KEY1='value'", "KEY2=  23"]
    length (keywords res) `shouldBe` 2
    Fits.lookup "KEY2" res `shouldBe` Just (Integer 23)

  it "should ignore comments" $ do
    res <- parse parseHeader $ flattenKeywords ["KEY1='value' / this is a comment"]
    length (keywords res) `shouldBe` 1
    Fits.lookup "KEY1" res `shouldBe` Just (String "value")

  it "should handle xtension" $ do
    res <- parse parseHeader $ flattenKeywords ["XTENSION= 'IMAGE   '"]
    length (keywords res) `shouldBe` 1
    Fits.lookup "XTENSION" res `shouldBe` Just (String "IMAGE")

  it "should parse blank line before keyword" $ do
    res <- parse parseHeader $ flattenKeywords [" ", "KEY2    = 22"]
    res.records `shouldBe` [BlankLine, Keyword (KeywordRecord "KEY2" (Integer 22) Nothing)]

  it "should parse line after keyword" $ do
    res <- parse parseHeader $ flattenKeywords ["KEY1    = 11", " ", "KEY2    = 22"]
    res.records `shouldBe` [Keyword (KeywordRecord "KEY1" (Integer 11) Nothing), BlankLine, Keyword (KeywordRecord "KEY2" (Integer 22) Nothing)]


requiredHeaders :: Spec
requiredHeaders =
  describe "required headers" $ do
    it "should parse bitpix" $ do
      res <- parse parseBitPix $ flattenKeywords ["BITPIX = 16"]
      res `shouldBe` SixteenBitInt

    it "should parse NAXES" $ do
      res <- parse parseNaxes $ flattenKeywords ["NAXIS   =3", "NAXIS1  =1", "NAXIS2  =2", "NAXIS3  =3"]
      res `shouldBe` [1, 2, 3]

    it "should parse size" $ do
      res <- parse parseDimensions $ flattenKeywords ["BITPIX = -32", "NAXIS=2", "NAXIS1=10", "NAXIS2=20"]
      res.bitpix `shouldBe` ThirtyTwoBitFloat
      res.axes `shouldBe` [10, 20]

    it "should include required headers in the keywords" $ do
      let fakeData = "1234" -- Related to NAXIS!
      h <- parse parsePrimary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> fakeData
      h.extension `shouldBe` Primary
      length (keywords h.header) `shouldBe` 5
      lookup "NAXIS" h.header `shouldBe` Just (Integer 2)

    it "should parse full extension" $ do
      h <- parse parseBinTable $ flattenKeywords ["XTENSION= 'BINTABLE'", "BITPIX = -32", "NAXIS=0", "PCOUNT=0", "GCOUNT=1"]
      h.extension `shouldBe` BinTable 0 ""


dataArray :: Spec
dataArray = describe "data array" $ do
  it "should grab correct data array" $ do
    let fakeData = "1234" -- Related to NAXIS!
    h <- parse parsePrimary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> "       " <> fakeData
    h.mainData `shouldBe` fakeData


sampleNSOHeaders :: Spec
sampleNSOHeaders = do
  describe "NSO Stripped Headers" $ do
    it "should parse comment block" $ do
      let hs =
            [ "DATASUM = '550335088'          / data unit checksum updated 2023-04-22T04:10:59 "
            , "   "
            , "COMMENT ------------------------------ Telescope -------------------------------"
            , "COMMENT  Keys describing the pointing and operation of the telescope. Including "
            , "COMMENT     the FITS WCS keys describing the world coordinates of the array.    "
            ]
      h <- parse parseHeader $ flattenKeywords hs
      length (keywords h) `shouldBe` 1

    describe "sample header file" $ do
      it "should parse all keywords individually" $ do
        SampleNSO ts <- getFixture
        forM_ (zip [1 :: Int ..] ts) $ \(_, t) -> do
          _ <- parse parseRecordLine $ flattenKeywords [TE.encodeUtf8 t]
          pure ()

      it "should parse xtension bintable" $ do
        DKISTHeaders bs <- getFixture
        (sz, _) <- parse parseBinTableKeywords $ mconcat $ C8.lines bs
        sz.axes `shouldBe` [32, 998]

      it "should parse NAXES correctly" $ do
        DKISTHeaders bs <- getFixture
        (sz, _) <- parse parseBinTableKeywords $ mconcat $ C8.lines bs
        sz.axes `shouldBe` [32, 998]


sampleNSO :: Spec
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse empty primary header" $ do
      DKISTFits bs <- getFixture
      h0 <- eitherFail $ readPrimaryHDU bs
      -- first header doesn't have any data
      BS.length h0.mainData `shouldBe` 0

    it "should parse both HDUs" $ do
      DKISTFits bs <- getFixture
      hdus <- eitherFail $ readHDUs bs
      length hdus `shouldBe` 2
      [_, h2] <- pure hdus
      Fits.lookup "INSTRUME" h2.header `shouldBe` Just (String "VISP")
      Fits.lookup "NAXIS" h2.header `shouldBe` Just (Integer 2)

      let countedHeaderBlocks = 11 -- this was manually counted... until end of all headers
          payloadLength = BS.length h2.mainData
          headerLength = countedHeaderBlocks * hduBlockSize
          -- sizeOnDisk = 161280
          heapLength = pCount h2.extension

      -- Payload size is as expected
      payloadLength `shouldBe` 32 * 998 * fromIntegral (bitPixToByteSize EightBitInt)
      pCount h2.extension `shouldBe` 95968

      if C8.all (/= '\0') (C8.take 100 (C8.drop (headerLength + payloadLength + heapLength - 100) bs))
        then pure ()
        else failTest "The end of the heap has some null data"

      if C8.all (== '\0') (C8.drop (headerLength + payloadLength + heapLength) bs)
        then pure ()
        else failTest "The remainder of the file contains real data"
 where
  pCount :: Extension -> Int
  pCount (BinTable p _) = p
  pCount _ = 0


newtype DKISTFits = DKISTFits BS.ByteString
instance Fixture DKISTFits where
  fixtureAction = do
    bs <- BS.readFile "./test/fits_files/nso_dkist.fits"
    pure $ noCleanup $ DKISTFits bs


newtype DKISTHeaders = DKISTHeaders BS.ByteString
instance Fixture DKISTHeaders where
  fixtureAction = do
    bs <- BS.readFile "./test/fits_files/nso_dkist_headers.txt"
    pure $ noCleanup $ DKISTHeaders bs


newtype SampleNSO = SampleNSO [Text]
instance Fixture SampleNSO where
  fixtureAction = do
    DKISTHeaders bs <- getFixture
    pure $ noCleanup $ SampleNSO $ filter (not . ignore) $ T.lines $ TE.decodeUtf8 bs
   where
    ignore t = T.isPrefixOf "CONTINUE" t || T.isPrefixOf "END" t
