{-# LANGUAGE OverloadedLists #-}

module Test.ArraySpec where

import Data.ByteString qualified as BS
import Data.Massiv.Array as M
import Data.Word (Word8)
import GHC.Int
import Skeletest
import System.ByteOrder
import Telescope.Data.Array
import Telescope.Data.Axes
import Telescope.Fits.DataArray


spec :: Spec
spec = do
  describe "indices" testIndices
  describe "decode" $ do
    -- describe "vector" testDecode
    describe "array" testDecodeArray
    describe "image" testDecodeImage

  describe "encode" $ do
    describe "array/image" testEncode


testIndices :: Spec
testIndices = do
  describe "axesIndex" $ do
    it "Ix1" $ do
      ix <- axesIndex $ toRowMajor $ Axes [10]
      ix `shouldBe` Ix1 10

    it "Ix2 in reverse order" $ do
      ix <- axesIndex $ toRowMajor $ Axes [3, 2]
      ix `shouldBe` 2 :. 3

    it "Ix3 in reverse order" $ do
      ix <- axesIndex $ toRowMajor $ Axes [3, 2, 1]
      ix `shouldBe` 1 :> 2 :. 3

  describe "indexAxes" $ do
    -- indexAxes produces row major
    it "Ix1" $ do
      indexAxes @Ix1 2 `shouldBe` Axes [2]

    it "Ix2 Row Major" $ do
      indexAxes (2 :. 3) `shouldBe` Axes [2, 3]

    it "Ix3 Row Major" $ do
      indexAxes (1 :> 2 :. 3) `shouldBe` Axes [1, 2, 3]

    it "Ix3 Column Major" $ do
      sizeAxes (Sz (1 :> 2 :. 3)) `shouldBe` Axes [3, 2, 1]


-- generate a series of Word8
genInput :: Word8 -> BS.ByteString
genInput start = BS.pack [start .. start + 100]


testDecodeArray :: Spec
testDecodeArray = do
  let input = genInput 0

  it "should parse int8 vector" $ do
    let v = decodeVector @Int8 Par BigEndian input
    computeAs P (M.take 6 v) `shouldBe` [0, 1, 2, 3, 4, 5]

  it "should decode Ix1 from vector" $ do
    let v = decodeVector @Int8 Par BigEndian input
    a <- fromVector @Ix1 (Axes [3]) $ M.take 3 v
    computeAs P a `shouldBe` [0, 1, 2]

  it "should decode Ix2" $ do
    a <- decodeArray @Ix2 @Int8 [2, 3] $ BS.take 6 input
    computeAs P a `shouldBe` [[0, 1, 2], [3, 4, 5]]

  it "should decode Ix3" $ do
    a <- decodeArray @Ix3 @Int8 [2, 2, 2] $ BS.take 8 input
    computeAs P a `shouldBe` [[[0, 1], [2, 3]], [[4, 5], [6, 7]]]


testDecodeImage :: Spec
testDecodeImage = do
  let input = genInput 0
  it "should decode image" $ do
    a <- decodeArray @Ix2 @Int8 [2, 3] $ BS.take 6 input
    computeAs P a `shouldBe` [[0, 1, 2], [3, 4, 5]]


testEncode :: Spec
testEncode = do
  let array = delay @Ix2 @P $ M.fromLists' Seq [[1, 2, 3], [4, 5, 6]] :: Array D Ix2 Int8

  it "should encode to 6 items" $ do
    BS.length (encodeArray array) `shouldBe` 6

  it "int8 should round-trip to original array" $ do
    let enc = encodeArray array :: BS.ByteString
    arr <- decodeArray @Ix2 @Int8 [2, 3] enc
    arr `shouldBe` array

  let arrayFloat = delay @Ix2 @P $ M.fromLists' Seq [[1, 2, 3], [4, 5, 6]] :: Array D Ix2 Float
  it "float should round-trip to original array" $ do
    let enc = encodeArray arrayFloat :: BS.ByteString
    arr <- decodeArray [2, 3] enc
    arr `shouldBe` arrayFloat

  let arrayInt = delay @Ix2 @P $ M.fromLists' Seq [[1, 2, 3], [4, 5, 6]] :: Array D Ix2 Int
  it "int should round-trip to original array" $ do
    let enc = encodeArray arrayInt :: BS.ByteString
    arr <- decodeArray [2, 3] enc
    arr `shouldBe` arrayInt

  it "should encode naxes in correct order" $ do
    size array `shouldBe` Sz (2 :. 3)
    sizeAxes (size array) `shouldBe` Axes [3, 2]
