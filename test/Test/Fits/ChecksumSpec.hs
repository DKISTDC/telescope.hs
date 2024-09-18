module Test.Fits.ChecksumSpec where

import Data.ByteString qualified as BS
import Data.Word
import Skeletest
import Telescope.Fits
import Telescope.Fits.Checksum
import Telescope.Fits.Encoding
import Telescope.Fits.Types


spec :: Spec
spec = do
  describe "checksum" specChecksum
  describe "add1s" specOnesComplement


specChecksum :: Spec
specChecksum = do
  describe "checksum" $ do
    it "should calc zeros" $ do
      let inp = BS.pack (replicate 100 0)
      checksum inp `shouldBe` Checksum 0

    it "should calc max" $ do
      let inp = BS.pack (replicate 100 (maxBound :: Word8))
      checksum inp `shouldBe` Checksum (maxBound :: Word32)

    it "should be equal with trailing zeros" $ do
      checksum (BS.pack [1, 2, 3, 4, 5]) `shouldBe` checksum (BS.pack [1, 2, 3, 4, 5, 0, 0, 0, 0, 0])

    it "should not be equal with trailing spaces" $ do
      checksum "12345" `shouldNotBe` checksum "12345     "

  describe "datasum" $ do
    it "should calc same datasum as astro.py" $ do
      inp <- BS.readFile "./samples/ones10x10.fits"
      f :: Fits <- decode inp

      let datasumHeader = lookupKeyword "DATASUM" f.primaryHDU.header
          datasumCalc = checksum f.primaryHDU.dataArray.rawData
          datasumCalcHeader = Just $ checksumValue datasumCalc
      datasumCalcHeader `shouldBe` datasumHeader

  describe "encoding" $ do
    it "checksum of empty HDU should be (-0)" $ do
      let empty = PrimaryHDU (Header []) (DataArray BPInt8 (Axes []) "")
      let out = encodePrimaryHDU empty
      checksum out `shouldBe` Checksum maxBound

    it "checksum of sample HDU should be (-0)" $ do
      inp <- BS.readFile "./samples/ones10x10.fits"
      f <- decode inp
      let out = encodePrimaryHDU f.primaryHDU
      checksum out `shouldBe` Checksum maxBound


specOnesComplement :: Spec
specOnesComplement = do
  describe "add" $ do
    it "max + 0" $ do
      add1s maxBound 0 `shouldBe` maxBound

    it "max + 1" $ do
      add1s maxBound 1 `shouldBe` 1

    it "max + max" $ do
      add1s maxBound maxBound `shouldBe` maxBound
