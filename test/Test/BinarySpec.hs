module Test.BinarySpec where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import GHC.Int
import Skeletest
import System.ByteOrder
import Telescope.Data.Binary


spec :: Spec
spec = do
  describe "BinaryValue" testBinaryValue


testBinaryValue :: Spec
testBinaryValue = do
  it "should decode int8" $ do
    get' @Int8 (put' @Int8 12) `shouldBe` 12
  it "should decode int16" $ do
    get' @Int16 (put' @Int16 12) `shouldBe` 12
  it "should decode int32" $ do
    get' @Int32 (put' @Int32 12) `shouldBe` 12
  it "should decode int64" $ do
    get' @Int64 (put' @Int64 12) `shouldBe` 12
  it "should decode Float" $ do
    get' @Float (put' @Float 12.21) `shouldBe` 12.21
  it "should decode Double" $ do
    get' @Double (put' @Double 12.21) `shouldBe` 12.21
  it "should decode Int" $ do
    get' @Int (put' @Int 12) `shouldBe` 12
 where
  get' :: (BinaryValue a) => ByteString -> a
  get' = runGet (get BigEndian)

  put' :: (BinaryValue a) => a -> ByteString
  put' a = runPut (put BigEndian a)
