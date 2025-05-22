{-# LANGUAGE TypeApplications #-}

module Test.Fits.EncodingSpec where

import Control.Monad.Catch (throwM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array qualified as M
import Data.Text (pack)
import Effectful.State.Static.Local
import Skeletest
import Telescope.Data.Axes
import Telescope.Fits
import Telescope.Fits.BitPix (bitPixBytes)
import Telescope.Fits.DataArray (Dimensions (..))
import Telescope.Fits.Encoding (mainData, parseThrow, runMega, runParseBytes)
import Telescope.Fits.Encoding qualified as Enc
import Telescope.Fits.Encoding.MegaParser qualified as MP
import Telescope.Fits.Encoding.Render hiding (justify, pad, spaces)
import Telescope.Fits.HDU.Block (hduBlockSize)
import Telescope.Fits.Header
import Test.Fits.MegaParserSpec (flattenKeywords)


spec :: Spec
spec = do
  describe "decode fits" testDecodeFits
  describe "render header" testRenderHeader
  describe "render data" testRenderData
  describe "encode primary" testEncodePrimary
  describe "round trip" testRoundTrip
  dataArraySpec
  fullHDUs
  simple2x3
  sampleNSO


testDecodeFits :: Spec
testDecodeFits = do
  describe "simple2x3.fits" $ do
    it "should parse primary" $ do
      Simple2x3Raw bs <- getFixture
      dm <- either throwM pure $ runParseBytes bs $ runMega "Primary Header" MP.parsePrimaryKeywords
      dm.axes `shouldBe` Axes [3, 2]

    it "should load metadata" $ do
      Simple2x3Raw bs <- getFixture
      f <- decode bs
      let dat = f.primaryHDU.dataArray
          hds = f.primaryHDU.header
      dat.axes `shouldBe` Axes [3, 2]
      dat.bitpix `shouldBe` BPInt64
      lookupKeyword "CUSTOM" hds `shouldBe` Just (Integer 123456)

    it "should load data array" $ do
      Simple2x3Raw bs <- getFixture
      f <- decode bs
      arr <- decodeDataArray @Ix2 @Int f.primaryHDU.dataArray
      M.toLists arr `shouldBe` [[0, 1, 2], [3, 4, 5]]

  describe "NSO L2 Data" $ do
    it "should load" $ do
      f <- decode =<< BS.readFile "samples/dkistL2.fits"
      length f.extensions `shouldBe` 1


-- arr <- decodeDataArray @Ix2 @Int f.primaryHDU.dataArray
-- M.toLists arr `shouldBe` [[0, 1, 2], [3, 4, 5]]

testRenderHeader :: Spec
testRenderHeader = do
  describe "renderValue" $ do
    it "int should right justify" $ do
      runValue (Integer 8) `shouldBe` justify 20 "8"

    it "float should right justify" $ do
      runValue (Float 3.2) `shouldBe` justify 20 "3.2"

    it "negative int" $ do
      runValue (Integer (-32)) `shouldBe` justify 20 "-32"

    it "negative float" $ do
      runValue (Float (-32.32)) `shouldBe` justify 20 "-32.32"

    it "float should exponent uppercase" $ do
      runValue (Float 6.0001e-16) `shouldBe` justify 20 "6.0001E-16"

    it "logic should right justify" $ do
      runValue (Logic T) `shouldBe` justify 20 "T"

    it "string" $ do
      runValue (String "Hello World") `shouldBe` "'Hello World'"

  -- TEST: does it matter if e-06 vs e-6? We output e-6.

  describe "renderKeyword" $ do
    it "should left justify" $ do
      run (renderKeyword "BITPIX") `shouldBe` "BITPIX  "

    it "should truncate" $ do
      run (renderKeyword "REALLYLONG") `shouldBe` "REALLYLO"

    it "should uppercase" $ do
      run (renderKeyword "lower") `shouldBe` "LOWER   "

  describe "renderKeywordValue" $ do
    it "should render space" $ do
      run (renderKeywordValue "SIMPLE" (Logic T)) `shouldBe` ("SIMPLE  = " <> justify 20 "T")

    it "should butt against equals" $ do
      run (renderKeywordValue "WHATEVER" (Integer 10)) `shouldBe` ("WHATEVER= " <> justify 20 "10")

    it "should string" $ do
      run (renderKeywordValue "WHATEVER" (String "dude")) `shouldBe` pad 30 "WHATEVER= 'dude'"

    it "should correctly count long values" $ do
      let render = renderKeywordValue "KEYWORD" (String "0123456789012345678901234567890123456789")
      render.length `shouldBe` length (run render)

  describe "renderComment" $ do
    it "should render comment" $ do
      run (renderComment 100 "Hello World") `shouldBe` " / Hello World"

    it "should truncate comment" $ do
      run (renderComment 10 "Hello World") `shouldBe` " / Hello W"

  describe "renderKeywordComments" $ do
    it "should render comment in line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) (Just "Comment")) `shouldBe` pad 80 ("SIMPLE  = " <> justify 20 "T" <> " / Comment")

    it "should render no comment" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) `shouldBe` pad 80 ("SIMPLE  = " <> justify 20 "T")

    it "should truncate whole line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) `shouldBe` pad 80 ("SIMPLE  = " <> justify 20 "T")

  describe "renderKeywordLine" $ do
    it "should be 80 characters mininum" $ do
      let b = renderKeywordLine "HELLO" (Integer 1) Nothing
      b.length `shouldBe` 80

    it "should be 80 characters maximum" $ do
      let b = renderKeywordLine "HELLO" (Integer 1) (Just $ pack $ replicate 100 'a')
      b.length `shouldBe` 80

    it "should be 80 characters maximum with long strings" $ do
      let b = renderKeywordLine "HELLO" (String "this is a really long value that exceeds 20") (Just $ pack $ replicate 100 'a')
      b.length `shouldBe` 80
      length (run b) `shouldBe` 80

    it "should be padded" $ do
      run (renderKeywordLine "HELLO" (Integer 1) Nothing) `shouldBe` "HELLO   = " <> justify 20 "1" <> spaces 50

  describe "renderOtherKeywords" $ do
    it "should render comments" $ do
      let h = Header [Comment "hello world"]
      run (renderOtherKeywords h) `shouldBe` pad 80 "COMMENT hello world"

    it "should render blanks" $ do
      let h = Header [BlankLine, Keyword (KeywordRecord "WOOT" (Integer 12345) Nothing)]
      run (renderOtherKeywords h) `shouldBe` headers ["", "WOOT    = " <> justify 20 "12345"]

    it "should render blanks between" $ do
      let h = Header [Comment "comment", BlankLine, Keyword (KeywordRecord "WOOT" (Integer 12345) Nothing)]
      run (renderOtherKeywords h) `shouldBe` headers ["COMMENT comment", "", "WOOT    = " <> justify 20 "12345"]
 where
  runValue :: Value -> String
  runValue = run . renderValue


run :: BuilderBlock -> String
run = C8.unpack . BL.toStrict . runRender


headers :: [String] -> String
headers = mconcat . map (pad 80)


justify :: Int -> String -> String
justify n s = spaces (n - length s) <> s


pad :: Int -> String -> String
pad n s = s <> spaces (n - length s)


spaces :: Int -> String
spaces n = replicate n ' '


testRenderData :: Spec
testRenderData = do
  it "fill block should fill" $ do
    (fillBlock zeros "a").length `shouldBe` 2880
    (fillBlock zeros "hello world").length `shouldBe` 2880
    (fillBlock zeros "").length `shouldBe` 0

  it "should be empty" $ do
    runRender (renderData "") `shouldBe` ""

  it "should pad to nearest block" $ do
    (renderData "asdf").length `shouldBe` 2880

  it "should render some data" $ do
    runRender (renderData "12345") `shouldBe` ("12345" <> BL.replicate 2875 0)


testEncodePrimary :: Spec
testEncodePrimary = do
  describe "encoded primary hdu" $ do
    it "encodes both a header and data hdu" $ do
      FitsEncodedFix enc <- getFixture
      BS.length enc `shouldBe` hduBlockSize * 2

    it "encodes the data" $ do
      FitsEncodedFix enc <- getFixture
      BS.take 6 (BS.drop 2880 enc) `shouldBe` rawData

    it "starts with SIMPLE" $ do
      FitsEncodedFix enc <- getFixture
      BS.take 30 enc `shouldBe` "SIMPLE  =                    T"

  describe "decoded encoded primary hdu" $ do
    it "Has custom header" $ do
      FitsDecodedFix f <- getFixture
      lookupKeyword "WOOT" f.primaryHDU.header `shouldBe` Just (Integer 123)

    it "Has required headers" $ do
      FitsDecodedFix f <- getFixture
      lookupKeyword "EXTEND" f.primaryHDU.header `shouldBe` Just (Logic T)

    it "Matches data metadata" $ do
      FitsDecodedFix f <- getFixture
      f.primaryHDU.dataArray.bitpix `shouldBe` BPInt8
      f.primaryHDU.dataArray.axes `shouldBe` Axes [3, 2]

    it "Matches raw data" $ do
      FitsDecodedFix f <- getFixture
      f.primaryHDU.dataArray.rawData `shouldBe` BS.pack [0 .. 5]
 where
  rawData = BS.pack [0 .. 5]


testRoundTrip :: Spec
testRoundTrip = do
  describe "simple2x3.fits" $ do
    it "should match metadata" $ do
      Simple2x3Fix fs <- getFixture
      f2 <- decode $ encode fs
      f2.primaryHDU.dataArray.axes `shouldBe` Axes [3, 2]
      f2.primaryHDU.dataArray.bitpix `shouldBe` fs.primaryHDU.dataArray.bitpix

    it "should match raw data" $ do
      Simple2x3Fix fs <- getFixture
      f2 <- decode $ encode fs
      f2.primaryHDU.dataArray.rawData `shouldBe` fs.primaryHDU.dataArray.rawData

    withMarkers ["focus"] $ it "should encode headers only once" $ do
      Simple2x3Fix fs <- getFixture
      f2 <- decode $ encode fs
      let hs = fs.primaryHDU.header
          h2 = f2.primaryHDU.header
      lookupKeyword "NAXIS" h2 `shouldBe` Just (Integer 2)

      let ks = keywords hs :: [KeywordRecord]
          k2 = keywords h2 :: [KeywordRecord]

      length (filter (matchKeyword "BITPIX") k2) `shouldBe` length (filter (matchKeyword "BITPIX") ks)

      length h2.records `shouldBe` length hs.records

    it "should keep naxes order preserved" $ do
      Simple2x3Fix fs <- getFixture
      f2 <- decode $ encode fs
      f2.primaryHDU.dataArray.axes `shouldBe` fs.primaryHDU.dataArray.axes

  describe "image fits" $ do
    it "should roundtrip image extensions" $ do
      let prim = DataHDU mempty emptyDataArray
      let img = Image $ DataHDU mempty emptyDataArray
      let out = encode $ Fits prim [img]
      fits2 <- decode out
      length fits2.extensions `shouldBe` 1
 where
  matchKeyword k (KeywordRecord k2 _ _) = k == k2


dataArraySpec :: Spec
dataArraySpec = describe "data array" $ do
  it "should parse fake data" $ do
    let fakeData = "1234" -- Related to NAXIS!
    d <- parseThrow (mainData (Dimensions BPInt8 (Axes [1, 4]))) fakeData
    d.rawData `shouldBe` fakeData

  it "should grab correct data array" $ do
    let fakeData = "1234" -- Related to NAXIS!
    h <- parseThrow Enc.primary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> "       " <> fakeData
    h.dataArray.rawData `shouldBe` fakeData


fullHDUs :: Spec
fullHDUs = describe "Full HDUs" $ do
  it "should include required headers in the keywords" $ do
    let fakeData = "1234" -- Related to NAXIS!
    h <- parseThrow Enc.primary $ flattenKeywords ["SIMPLE = T", "BITPIX = 8", "NAXIS=2", "NAXIS1=2", "NAXIS2=2", "TEST='hi'"] <> fakeData
    length (keywords h.header) `shouldBe` 6
    lookupKeyword "NAXIS" h.header `shouldBe` Just (Integer 2)

  it "should parse full extension" $ do
    bt <- parseThrow Enc.binTable $ flattenKeywords ["XTENSION= 'BINTABLE'", "BITPIX = -32", "NAXIS=0", "PCOUNT=0", "GCOUNT=1"]
    bt.pCount `shouldBe` 0
    bt.heap `shouldBe` ""


simple2x3 :: Spec
simple2x3 = do
  describe "simple2x3.fits" $ do
    it "should parse primary" $ do
      Simple2x3Raw bs <- getFixture
      p <- parseThrow Enc.primary bs
      p.dataArray.axes `shouldBe` Axes [3, 2]


sampleNSO :: Spec
sampleNSO = do
  describe "NSO Sample FITS Parse" $ do
    it "should parse primary HDU" $ do
      DKISTFitsRaw bs <- getFixture
      p <- parseThrow Enc.primary bs
      BS.length p.dataArray.rawData `shouldBe` 0

    it "should parse BINTABLE" $ do
      DKISTFitsRaw bs <- getFixture
      (bt :: BinTableHDU, rest) <- flip parseThrow bs $ do
        bt <- Enc.primary >> Enc.binTable
        rest <- get @BS.ByteString
        pure (bt, rest)
      lookupKeyword "INSTRUME" bt.header `shouldBe` Just (String "VISP")
      lookupKeyword "NAXIS" bt.header `shouldBe` Just (Integer 2)

      let countedHeaderBlocks = 11 -- this was manually counted... until end of all headers
          payloadLength :: Int = BS.length bt.dataArray.rawData
          headerLength :: Int = countedHeaderBlocks * hduBlockSize
          -- sizeOnDisk = 161280
          heapLength :: Int = bt.pCount

      -- Payload size is as expected
      payloadLength `shouldBe` 32 * 998 * fromIntegral (bitPixBytes BPInt8)
      bt.pCount `shouldBe` 95968

      if C8.all (/= '\0') (C8.take 100 (C8.drop (headerLength + payloadLength + heapLength - 100) rest))
        then pure ()
        else failTest "The end of the heap has some null data"

      if C8.all (== '\0') (C8.drop (headerLength + payloadLength + heapLength) rest)
        then pure ()
        else failTest "The remainder of the file contains real data"


newtype FitsDecodedFix = FitsDecodedFix Fits
instance Fixture FitsDecodedFix where
  fixtureAction = do
    FitsEncodedFix enc <- getFixture
    f <- decode enc
    pure $ noCleanup $ FitsDecodedFix f


newtype Simple2x3Raw = Simple2x3Raw BS.ByteString
instance Fixture Simple2x3Raw where
  fixtureAction = do
    bs <- BS.readFile "samples/simple2x3.fits"
    pure $ noCleanup $ Simple2x3Raw bs


newtype DKISTFitsRaw = DKISTFitsRaw BS.ByteString
instance Fixture DKISTFitsRaw where
  fixtureAction = do
    bs <- BS.readFile "./samples/nso_dkist.fits"
    pure $ noCleanup $ DKISTFitsRaw bs


newtype Simple2x3Fix = Simple2x3Fix Fits
instance Fixture Simple2x3Fix where
  fixtureAction = do
    inp <- BS.readFile "samples/simple2x3.fits"
    fits <- decode inp
    pure $ noCleanup $ Simple2x3Fix fits


newtype FitsEncodedFix = FitsEncodedFix BS.ByteString
instance Fixture FitsEncodedFix where
  fixtureAction = do
    pure $ noCleanup $ FitsEncodedFix encoded
   where
    encoded = encode (Fits primary [])
    primary =
      let heads = Header [Keyword $ KeywordRecord "WOOT" (Integer 123) Nothing]
          dat = DataArray BPInt8 (Axes [3, 2]) $ BS.pack [0 .. 5]
       in DataHDU heads dat
