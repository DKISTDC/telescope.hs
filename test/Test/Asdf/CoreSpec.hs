module Test.Asdf.CoreSpec where

import Skeletest
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Node
import Telescope.Data.Parser


spec :: Spec
spec = do
  describe "basics" $ do
    it "basic encode" $ do
      toValue Count `shouldBe` String "count"
      toValue Kilometers `shouldBe` String "km"
      toValue Seconds `shouldBe` String "s"

    it "basic decode" $ do
      testParseEq (String "count") Count
      testParseEq (String "km") Kilometers
      testParseEq (String "s") Seconds

  describe "product" $ do
    it "encodes with ." $ do
      toValue (Product Kilometers Seconds) `shouldBe` "km.s"

    it "decodes from ." $ do
      testParseEq "km.s" (Product Kilometers Seconds)

  describe "exponent" $ do
    it "encodes with **" $ do
      toValue (Exponent 2 Seconds) `shouldBe` "s**2"

    it "decodes from **n" $ do
      testParseEq "s**2" (Exponent 2 Seconds)

    it "encodes negative exponents" $ do
      toValue (Exponent (-2) Seconds) `shouldBe` "s**-2"

    it "decodes from **-n" $ do
      testParseEq "s**-2" (Exponent (-2) Seconds)

    it "should collapse to unit" $ do
      toValue (Exponent 1 Seconds) `shouldBe` toValue Seconds

  describe "products with exponents" $ do
    it "products with exponents" $ do
      toValue (Product Kilometers (Exponent 2 Seconds)) `shouldBe` "km.s**2"
      toValue (Product Kilometers (Exponent (-2) Seconds)) `shouldBe` "km.s**-2"

    it "decodes products with exponents" $ do
      testParseEq "km.s**2" (Product Kilometers (Exponent 2 Seconds))
      testParseEq "km.s**-2" (Product Kilometers (Exponent (-2) Seconds))


testParseEq :: forall a m. (Testable m, Eq a, FromAsdf a) => Value -> a -> m ()
testParseEq v expect = do
  let res :: Either ParseError a = runParserPure $ parseValue v
  res `shouldBe` Right expect
