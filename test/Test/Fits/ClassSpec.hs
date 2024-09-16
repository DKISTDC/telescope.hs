module Test.Fits.ClassSpec where

import Data.Text (Text)
import GHC.Generics
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Data.Parser
import Telescope.Fits qualified as Fits
import Telescope.Fits.Header


newtype Activity = Activity Text
  deriving newtype (FromKeyword)


instance ToKeyword Activity where
  toValue (Activity t) = toValue t
  toKeyword _ = Just "activity"


-- toKeywordRecord _ a = KeywordRecord "activity" (toKeywordValue a) Nothing

data Test = Test
  { age :: Int
  , name :: Text
  }
  deriving (Generic, ToHeader, FromHeader)


data Test2 = Test2
  { hobby :: Activity
  }
  deriving (Generic, ToHeader, FromHeader)


spec :: Spec
spec = do
  describe "To/From KeywordValue" $ do
    it "should toKeywordValue" $ do
      toValue (23 :: Int) `shouldBe` Integer 23
      toValue ("hello" :: Text) `shouldBe` String "hello"
      toValue True `shouldBe` Logic T

    it "should parseKeywordValue" $ do
      runParser (parseKeywordValue @Int $ Integer 23) `shouldBe` Right 23
      runParser (parseKeywordValue @Text $ String "woot") `shouldBe` Right "woot"
      runParser (parseKeywordValue @Bool $ Logic F) `shouldBe` Right False

  describe "ToHeader" $ do
    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      Fits.lookup "age" h `shouldBe` Just (Integer 40)
      Fits.lookup "name" h `shouldBe` Just (String "Alice")

    it "Uses custom keyword name from members of ToHeader" $ do
      let h = toHeader (Test2 (Activity "dancing"))
      Fits.lookup "hobby" h `shouldBe` Nothing
      Fits.lookup "activity" h `shouldBe` Just (String "dancing")

  describe "FromHeader" $ do
    it "should convert datatype" $ do
      let h = toHeader (Test 40 "Alice")
      let et = runParser $ parseHeader h
      et `shouldSatisfy` P.right (P.con Test{age = P.eq 40})
      et `shouldSatisfy` P.right (P.con Test{name = P.eq "Alice"})
