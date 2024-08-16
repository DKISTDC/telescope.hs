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
  it "should decode ints" $ do
    get' @Int8 (put' @Int8 12) `shouldBe` 12
    get' @Int16 (put' @Int16 12) `shouldBe` 12
    get' @Int32 (put' @Int32 12) `shouldBe` 12
    get' @Int64 (put' @Int64 12) `shouldBe` 12
    get' @Int (put' @Int 12) `shouldBe` 12
  it "should decode floats" $ do
    get' @Float (put' @Float 12.21) `shouldBe` 12.21
    get' @Double (put' @Double 12.21) `shouldBe` 12.21
 where
  get' :: (BinaryValue a) => ByteString -> a
  get' = runGet (get BigEndian)

  put' :: (BinaryValue a) => a -> ByteString
  put' a = runPut (put BigEndian a)
