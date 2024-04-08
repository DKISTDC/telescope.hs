{-# LANGUAGE OverloadedLists #-}

module Test.ArraySpec where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array as M
import Data.Word (Word8)
import GHC.Int
import Telescope.Fits.DataArray
import Telescope.Fits.Types
import Test.Syd


spec :: Spec
spec = do
  describe "indices" testIndices
  describe "getPix" testGetPix
  describe "decode" $ do
    describe "vector" testDecode
    describe "array" testDecodeArray
    describe "image" testDecodeImage


-- describe "encode" $ do
-- describe "array/image" $testEncode

testIndices :: Spec
testIndices = do
  it "Ix1" $ do
    ix <- axesIndex $ rowMajor $ Axes [10]
    ix `shouldBe` Ix1 10

  it "Ix2 in reverse order" $ do
    ix <- axesIndex $ rowMajor $ Axes [3, 2]
    ix `shouldBe` 2 :. 3

  it "Ix3 in reverse order" $ do
    ix <- axesIndex $ rowMajor $ Axes [3, 2, 1]
    ix `shouldBe` 1 :> 2 :. 3


testGetPix :: Spec
testGetPix = do
  -- it "should decode float" $ do
  --   let inp = [63,153,153,154] :: [Word8]
  --   runGet (getPix @Float ThirtyTwoBitFloat) (BL.pack inp) `shouldBe` 1.2

  it "should decode float" $ do
    let inp = runPut (putFloatbe 1.2)
    runGet (getPix @Float BPFloat) inp `shouldBe` 1.2

  it "should decode double" $ do
    let inp = runPut (putDoublebe 1.2)
    runGet (getPix @Double BPDouble) inp `shouldBe` 1.2

  it "should decode int8" $ do
    let inp = runPut (putInt8 12)
    runGet (getPix @Int8 BPInt8) inp `shouldBe` 12

  it "should decode int16" $ do
    let inp = runPut (putInt16be 12)
    runGet (getPix @Int16 BPInt16) inp `shouldBe` 12

  it "should decode int32" $ do
    let inp = runPut (putInt32be 12)
    runGet (getPix @Int32 BPInt32) inp `shouldBe` 12

  it "should decode int64" $ do
    let inp = runPut (putInt64be 12)
    runGet (getPix @Int64 BPInt64) inp `shouldBe` 12

  it "should decode int 8" $ do
    runGet (getPix @Int BPInt8) (runPut (putInt8 12)) `shouldBe` 12
  it "should decode int 16" $ do
    runGet (getPix @Int BPInt16) (runPut (putInt16be 12)) `shouldBe` 12
  it "should decode int 32" $ do
    runGet (getPix @Int BPInt32) (runPut (putInt32be 12)) `shouldBe` 12
  it "should decode int 64" $ do
    runGet (getPix @Int BPInt64) (runPut (putInt64be 12)) `shouldBe` 12


genInput :: Word8 -> BS.ByteString
genInput start = BS.pack [start .. start + 100]


testDecode :: Spec
testDecode = do
  let input = genInput 0

  it "should calc total pixels" $ do
    totalPix (Axes [3, 2, 2]) `shouldBe` 12

  it "should get pixels" $ do
    px <- runGetThrow (getAxesVector (getPix BPInt8) (Axes [5])) $ BL.fromStrict input
    let arr = compute px :: Array P Ix1 Int
    arr `shouldBe` [0, 1, 2, 3, 4]

  it "should get pixels 2d" $ do
    px <- runGetThrow (getAxesVector (getPix BPInt8) (Axes [3, 2])) $ BL.fromStrict input
    let arr = compute px :: Array P Ix1 Int
    arr `shouldBe` [0, 1, 2, 3, 4, 5]


testDecodeArray :: Spec
testDecodeArray = do
  let input = genInput 0

  it "should parse vector" $ do
    let v = parseVector @Int Par BPInt8 input
    computeAs P (M.take 6 v) `shouldBe` [0, 1, 2, 3, 4, 5]

  it "should decode Ix1 from vector" $ do
    let v = parseVector @Int Par BPInt8 input
    a <- fromVector @Ix1 (Axes [3]) $ M.take 3 v
    computeAs P a `shouldBe` [0, 1, 2]

  it "should decode Ix2" $ do
    a <- decodeArray' @Ix2 @Int BPInt8 (Axes [3, 2]) $ BS.take 6 input
    computeAs P a `shouldBe` [[0, 1, 2], [3, 4, 5]]

  it "should decode Ix3" $ do
    a <- decodeArray' @Ix3 @Int BPInt8 (Axes [2, 2, 2]) $ BS.take 8 input
    computeAs P a `shouldBe` [[[0, 1], [2, 3]], [[4, 5], [6, 7]]]


testDecodeImage :: Spec
testDecodeImage = do
  let input = genInput 0
  it "should decode image" $ do
    a <- decodeArray' @Ix2 @Int BPInt8 (Axes [3, 2]) $ BS.take 6 input
    computeAs P a `shouldBe` [[0, 1, 2], [3, 4, 5]]


testEncode :: Spec
testEncode = do
  let array = delay @Ix2 @P $ M.fromLists' Seq [[1, 2, 3], [4, 5, 6]] :: Array D Ix2 Int8

  it "should encode to 6 items" $ do
    BS.length (encodeArray' array) `shouldBe` 6

  it "int8 should round-trip to original array" $ do
    let enc = encodeArray' array :: BS.ByteString
    arr <- decodeArray' @Ix2 @Int8 BPInt8 (Axes [3, 2]) enc
    arr `shouldBe` array

  let arrayFloat = delay @Ix2 @P $ M.fromLists' Seq [[1, 2, 3], [4, 5, 6]] :: Array D Ix2 Float
  it "float should round-trip to original array" $ do
    let enc = encodeArray' arrayFloat :: BS.ByteString
    arr <- decodeArray' @Ix2 @Float BPFloat (Axes [3, 2]) enc
    arr `shouldBe` arrayFloat

-- TODO: reimplement
-- it "should encode images" $ do
--   let dim = Dimensions ThirtyTwoBitFloat [3, 2]
--       img = encodeImage arrayFloat
--   arr2 <- decodeImage img
--   arr2 `shouldBe` arrayFloat
