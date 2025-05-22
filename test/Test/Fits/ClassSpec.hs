module Test.Fits.ClassSpec where

import Control.Monad.Catch (throwM)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import GHC.Generics
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Data.Axes
import Telescope.Data.Parser
import Telescope.Data.WCS
import Telescope.Fits.Header


spec :: Spec
spec = do
  describe "class" classSpec
  describe "wcs" wcsSpec


newtype Activity = Activity Text
  deriving newtype (FromKeyword, ToKeyword)


data Test = Test
  { age :: Int
  , firstName :: Text
  }
  deriving (Generic, ToHeader, FromHeader)


data Woot = Woot
  { woot :: Float
  }
  deriving (Generic, ToHeader)


data Parent = Parent
  { test :: HeaderFor Test
  , woot :: HeaderFor Woot
  }
  deriving (Generic, ToHeader)


classSpec :: Spec
classSpec = do
  describe "To/From KeywordValue" $ do
    it "should toKeywordValue" $ do
      toKeywordValue (23 :: Int) `shouldBe` Integer 23
      toKeywordValue ("hello" :: Text) `shouldBe` String "hello"
      toKeywordValue True `shouldBe` Logic T

    it "should parseKeywordValue" $ do
      runPureEff (runParser (parseKeywordValue @Int $ Integer 23)) `shouldBe` Right 23
      runPureEff (runParser (parseKeywordValue @Text $ String "woot")) `shouldBe` Right "woot"
      runPureEff (runParser (parseKeywordValue @Bool $ Logic F)) `shouldBe` Right False

  describe "ToHeader" $ do
    it "should uppercase and snake keywords" $ do
      let h = toHeader (Test 40 "Alice")
      length h.records `shouldBe` 2
      [Keyword (KeywordRecord k1 _ _), Keyword (KeywordRecord k2 _ _)] <- pure h.records
      k1 `shouldBe` "AGE"
      k2 `shouldBe` "FIRST_NAME"

    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      lookupKeyword "AGE" h `shouldBe` Just (Integer 40)
      lookupKeyword "FIRST_NAME" h `shouldBe` Just (String "Alice")

  it "should mconcat fields that are members of toHeader" $ do
    let h = toHeader $ Parent (HeaderFor $ Test 40 "Alice") (HeaderFor $ Woot 123.456)
    lookupKeyword "age" h `shouldBe` Just (Integer 40)
    lookupKeyword "first_name" h `shouldBe` Just (String "Alice")
    lookupKeyword "woot" h `shouldSatisfy` P.just (P.con (Float (P.approx P.tol 123.456)))

  describe "FromHeader" $ do
    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      let et = runPureEff $ runParser $ parseHeader h
      et `shouldSatisfy` P.right (P.con Test{age = P.eq 40})
      et `shouldSatisfy` P.right (P.con Test{firstName = P.eq "Alice"})


data X
instance AxisOrder X where
  axisN = 1


data Y
instance AxisOrder Y where
  axisN = 2


wcsSpec :: Spec
wcsSpec = do
  describe "axis To/From Header" $ do
    it "should incorporate axis order into keywords" $ do
      let hx = toHeader wcsX
      lookupKeyword "CRPIX1" hx `shouldBe` Just (Float 1.0)

    it "should incorporate wcsalt into keywords" $ do
      let h = toHeader wcsAY
      lookupKeyword "CRVAL2A" h `shouldBe` Just (Float 5.0)

    it "should roundtrip" $ do
      let h = toHeader wcsAY
      wcs2 <- parseIO $ parseHeader h
      wcs2 `shouldBe` wcsAY
 where
  wcsX = WCSAxis (CType "X") (CUnit "M") 1 2 3 :: WCSAxis 'WCSMain X
  wcsAY = WCSAxis (CType "Y") (CUnit "M") 4 5 6 :: WCSAxis 'A Y


parseIO :: Eff '[Parser, IOE] a -> IO a
parseIO p = do
  res <- runEff $ runParser p
  case res of
    Left e -> throwM e
    Right a -> pure a
