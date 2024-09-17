module Test.Fits.ClassSpec where

import Data.Text (Text)
import GHC.Generics
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Data.Parser
import Telescope.Fits qualified as Fits
import Telescope.Fits.Header


newtype Activity = Activity Text
  deriving newtype (FromKeyword, ToKeyword)


-- toKeywordRecord _ a = KeywordRecord "activity" (toKeywordValue a) Nothing

data Test = Test
  { age :: Int
  , firstName :: Text
  }
  deriving (Generic, ToHeader, FromHeader)


data Woot = Woot
  { woot :: Float
  }


data Parent = Parent
  { test :: Test
  , woot :: Woot
  }
  deriving (Generic)


spec :: Spec
spec = do
  describe "To/From KeywordValue" $ do
    it "should toKeywordValue" $ do
      toKeywordValue (23 :: Int) `shouldBe` Integer 23
      toKeywordValue ("hello" :: Text) `shouldBe` String "hello"
      toKeywordValue True `shouldBe` Logic T

    it "should parseKeywordValue" $ do
      runParser (parseKeywordValue @Int $ Integer 23) `shouldBe` Right 23
      runParser (parseKeywordValue @Text $ String "woot") `shouldBe` Right "woot"
      runParser (parseKeywordValue @Bool $ Logic F) `shouldBe` Right False

  describe "ToHeader" $ do
    it "should uppercase and snake keywords" $ do
      let h = toHeader (Test 40 "Alice")
      length h._records `shouldBe` 2
      [Keyword (KeywordRecord k1 _ _), Keyword (KeywordRecord k2 _ _)] <- pure h._records
      k1 `shouldBe` "AGE"
      k2 `shouldBe` "FIRST_NAME"

    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      lookupKeyword "AGE" h `shouldBe` Just (Integer 40)
      lookupKeyword "FIRST_NAME" h `shouldBe` Just (String "Alice")

  -- it "should mconcat fields that are members of toHeader" $ do
  --   let h = toHeader $ Parent (Test 40 "Alice") (Woot 123.456)
  --   lookupKeyword "age" h `shouldBe` Just (Integer 40)
  --   lookupKeyword "first_name" h `shouldBe` Just (String "Alice")
  --   lookupKeyword "woot" h `shouldSatisfy` P.just (P.con (Float (P.eq 123.456)))

  describe "FromHeader" $ do
    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      let et = runParser $ parseHeader h
      et `shouldSatisfy` P.right (P.con Test{age = P.eq 40})
      et `shouldSatisfy` P.right (P.con Test{firstName = P.eq "Alice"})
