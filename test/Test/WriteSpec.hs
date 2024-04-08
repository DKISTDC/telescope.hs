module Test.WriteSpec where

import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text (pack)
import Telescope.Fits.Encoding hiding (justify, pad, spaces)
import Telescope.Fits.Types
import Test.Syd


spec :: Spec
spec = do
  -- describe "gen header" testGenHeader
  describe "render header" testRenderHeader
  describe "render data" testRenderData


-- testGenHeader :: Test ()
-- testGenHeader = do
--   describe "mandatory" $ do
--     let h = headerMandatory (Dimensions EightBitInt [3,2])
--
--     it "main" $ do
--       keywordEquals "SIMPLE" (Logic T) h
--       keywordEquals "BITPIX" (Integer 8) h
--
--     it "NAXIS" $ do
--       keywordEquals "NAXIS" (Integer 2) h
--
--     it "NAXISn" $ do
--       keywordEquals (naxis 1) (Integer 3) h
--       keywordEquals (naxis 2) (Integer 2) h
--
--   where
--     naxis n = Keyword $ "NAXIS" <> pack (show n)
--
--     keywordEquals :: Keyword -> Value -> Header -> IO ()
--     keywordEquals k v h = do
--       vx <- getKeyword' k h
--       vx `shouldBe` v

testRenderHeader :: Spec
testRenderHeader = do
  describe "renderValue" $ do
    it "int should right justify" $ do
      runValue (Integer 8) `shouldBe` justify 30 "8"

    it "float should right justify" $ do
      runValue (Float 3.2) `shouldBe` justify 30 "3.2"

    it "negative int" $ do
      runValue (Integer (-32)) `shouldBe` justify 30 "-32"

    it "negative float" $ do
      runValue (Float (-32.32)) `shouldBe` justify 30 "-32.32"

    it "float should exponent uppercase" $ do
      runValue (Float 6.0001e-16) `shouldBe` justify 30 "6.0001E-16"

    it "logic should right justify" $ do
      runValue (Logic T) `shouldBe` justify 30 "T"

    it "string" $ do
      runValue (String "Hello World") `shouldBe` "'Hello World'"

  -- TODO: does it matter if e-06 vs e-6? We output e-6.

  describe "renderKeyword" $ do
    it "should left justify" $ do
      run (renderKeyword "BITPIX") `shouldBe` "BITPIX  "

    it "should truncate" $ do
      run (renderKeyword "REALLYLONG") `shouldBe` "REALLYLO"

    it "should uppercase" $ do
      run (renderKeyword "lower") `shouldBe` "LOWER   "

  describe "renderKeywordValue" $ do
    it "should render space" $ do
      run (renderKeywordValue "SIMPLE" (Logic T)) `shouldBe` ("SIMPLE  = " <> justify 30 "T")

    it "should butt against equals" $ do
      run (renderKeywordValue "WHATEVER" (Integer 10)) `shouldBe` ("WHATEVER= " <> justify 30 "10")

    it "should string" $ do
      run (renderKeywordValue "WHATEVER" (String "dude")) `shouldBe` pad 40 "WHATEVER= 'dude'"

  describe "renderComment" $ do
    it "should render comment" $ do
      run (renderComment 100 (Comment "Hello World")) `shouldBe` " / Hello World"

    it "should truncate comment" $ do
      run (renderComment 10 (Comment "Hello World")) `shouldBe` " / Hello W"

  describe "renderKeywordComments" $ do
    it "should render comment in line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) (Just "Comment")) `shouldBe` pad 80 ("SIMPLE  = " <> justify 30 "T" <> " / Comment")

    it "should render no comment" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) `shouldBe` pad 80 ("SIMPLE  = " <> justify 30 "T")

    it "should truncate whole line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) `shouldBe` pad 80 ("SIMPLE  = " <> justify 30 "T")

  describe "renderKeywordLine" $ do
    it "should be 80 characters mininum" $ do
      let b = renderKeywordLine "HELLO" (Integer 1) Nothing
      b.length `shouldBe` 80

    it "should be 80 characters maximum" $ do
      let b = renderKeywordLine "HELLO" (Integer 1) (Just $ Comment $ pack $ replicate 100 'a')
      b.length `shouldBe` 80

    it "should be padded" $ do
      run (renderKeywordLine "HELLO" (Integer 1) Nothing) `shouldBe` "HELLO   = " <> justify 30 "1" <> spaces 40
 where
  run :: BuilderBlock -> String
  run = C8.unpack . runRender

  runValue :: Value -> String
  runValue = run . renderValue

  justify :: Int -> String -> String
  justify n s = spaces (n - length s) <> s

  pad :: Int -> String -> String
  pad n s = s <> spaces (n - length s)

  spaces :: Int -> String
  spaces n = replicate n ' '


testRenderData :: Spec
testRenderData = do
  it "fill block should fill" $ do
    (fillBlock "a").length `shouldBe` 2880
    (fillBlock "hello world").length `shouldBe` 2880
    (fillBlock "").length `shouldBe` 0

  it "should be empty" $ do
    runRender (renderData "") `shouldBe` ""

  it "should pad to nearest block" $ do
    (renderData "asdf").length `shouldBe` 2880
