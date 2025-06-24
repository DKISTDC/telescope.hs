module Test.DataCubeSpec where


import Data.Massiv.Array as M
import Skeletest
import Telescope.Data.DataCube

data Row
data Column
data Depth


spec :: Spec
spec = do
  describe "DataCube" $ do
    describe "assumptions" $ do
      it "should have expected size" $ do
        M.size sampleRC.array `shouldBe` Sz (2 :. 3)

      it "should have expected values" $ do
        M.toLists sampleRC.array `shouldBe` [[0, 1, 2], [1, 2, 3]]

    describe "transpose" $ do
      it "should swap two dimensions" $ do
        M.size (transposeMajor sampleRC).array `shouldBe` Sz (3 :. 2)
        toLists (computeAs P (transposeMajor sampleRC).array) `shouldBe` [[0, 1], [1, 2], [2, 3]]

      it "should swap three dimensions" $ do
        M.size (transposeMajor sampleDRC).array `shouldBe` Sz (2 :> 1 :. 3)

    describe "slice major 0" $ do
      it "should row access" $ do
        toLists (computeAs P (sliceM0 0 sampleRC).array) `shouldBe` [0.0, 1.0, 2.0]

      it "should depth access" $ do
        toLists (computeAs P (sliceM0 0 sampleDRC).array) `shouldBe` [[0.0, 1.0, 2.0], [1.0, 2.0, 3.0]]

    describe "slice major 1" $ do
      it "should slice square column" $ do
        toLists (computeAs P (sliceM1 0 sampleRC).array) `shouldBe` [0.0, 1.0]
        toLists (computeAs P (sliceM1 1 sampleRC).array) `shouldBe` [1.0, 2.0]

      it "should slice cube by row" $ do
        -- print sampleDRC
        size (sliceM1 0 sampleDRC).array `shouldBe` Sz (1 :. 3)
        -- \| 0 1 2 |
        -- \| 1 2 3 |
        toLists (computeAs P (sliceM1 0 sampleDRC).array) `shouldBe` [[0.0, 1.0, 2.0]]

    describe "slice major 2" $ do
      it "should slice cube by column" $ do
        size (sliceM2 0 sampleDRC).array `shouldBe` Sz (1 :. 2)
        toLists (computeAs P (sliceM2 2 sampleDRC).array) `shouldBe` [[2.0, 3.0]]


sampleRC :: DataCube [Row, Column] Float
sampleRC = DataCube $ M.makeArray Seq (Sz (2 :. 3)) sumIndex
 where
  sumIndex (r :. c) = fromIntegral r + fromIntegral c


sampleDRC :: DataCube [Depth, Row, Column] Float
sampleDRC = DataCube $ M.makeArray @D @Ix3 Seq (Sz (1 :> 2 :. 3)) sumIndex
 where
  sumIndex (d :> r :. c) = fromIntegral d + fromIntegral r + fromIntegral c
