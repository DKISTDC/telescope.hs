module Test.ChecksumSpec where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word
import Telescope.Fits qualified as Fits (lookup)
import Telescope.Fits.Checksum
import Telescope.Fits.Encoding
import Telescope.Fits.Types
import Test.Syd


spec :: Spec
spec = do
  -- describe "ones complement" testOnes
  -- describe "datasum" testDatasum
  describe "checksum" testChecksum


-- testOnes :: Spec
-- testOnes = do
--   describe "Word8" $ do
--     it "max + 0" $ do
--       add1s @Word8 maxBound 0 `shouldBe` maxBound
--
--     it "max + 1" $ do
--       add1s @Word8 maxBound 1 `shouldBe` 1
--
--     it "max + max" $ do
--       add1s @Word8 maxBound maxBound `shouldBe` maxBound
--
--   describe "Word32" $ do
--     it "max + 0" $ do
--       add1s @Word32 maxBound 0 `shouldBe` maxBound
--
--     it "max + 1" $ do
--       add1s @Word32 maxBound 1 `shouldBe` 1
--
--     it "max-1 + 1" $ do
--       add1s @Word32 (maxBound - 1) 1 `shouldBe` maxBound
--
--   describe "sum32" $ do
--     it "empty" $ do
--       sum32 [] `shouldBe` 0
--
--     it "singleton" $ do
--       sum32 [maxBound] `shouldBe` maxBound
--
--     it "sums" $ do
--       sum32 [maxBound, maxBound - 1, 1] `shouldBe` maxBound
--
--     it "zeros" $ do
--       sum32 (replicate 10 0) `shouldBe` 0
--
--     it "0b1111-" $ do
--       sum32 (replicate 10 maxBound) `shouldBe` maxBound
--
--   describe "add1s32" $ do
--     it "max + 1" $ do
--       add1s' maxBound 1 `shouldBe` 1
--
--     it "max + 0" $ do
--       add1s' maxBound 0 `shouldBe` maxBound

-- testDatasum :: Spec
-- testDatasum = do
--   it "should calc zeros" $ do
--     let inp = BS.pack (replicate 100 0)
--     datasum inp `shouldBe` "0"
--
--   it "should calc max" $ do
--     let inp = BS.pack (replicate 100 (maxBound :: Word8))
--     datasum inp `shouldBe` T.pack (show (maxBound :: Word32))
--
--   it "should calc same datasum as astro.py" $ do
--     inp <- BS.readFile "./samples/ones10x10.fits"
--     f :: Fits <- decode inp
--     let datasumHeader = Fits.lookup "DATASUM" f.primaryHDU.header
--         datasumCalc = datasum f.primaryHDU.dataArray.rawData
--     Just (String datasumCalc) `shouldBe` datasumHeader

testChecksum :: Spec
testChecksum = do
  it "should calc zeros" $ do
    let inp = BS.pack (replicate 100 0)
    checksum inp `shouldBe` 0

  it "should calc max" $ do
    let inp = BS.pack (replicate 100 (maxBound :: Word8))
    checksum inp `shouldBe` (maxBound :: Word32)

  it "should be equal with trailing zeros" $ do
    checksum (BS.pack [1, 2, 3, 4, 5]) `shouldBe` checksum (BS.pack [1, 2, 3, 4, 5, 0, 0, 0, 0, 0])

  it "should not be equal with trailing spaces" $ do
    checksum "12345" `shouldNotBe` checksum "12345     "

  it "should calc same datasum as astro.py" $ do
    inp <- BS.readFile "./samples/ones10x10.fits"
    f :: Fits <- decode inp

    let datasumHeader = Fits.lookup "DATASUM" f.primaryHDU.header
        datasumCalc = checksum f.primaryHDU.dataArray.rawData
        datasumCalcHeader = Just $ String $ T.pack $ show datasumCalc
    datasumCalcHeader `shouldBe` datasumHeader
