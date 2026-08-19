module Test.Asdf.NDArraySpec where

import Control.Monad (replicateM)
import Data.Binary.Get (runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import GHC.Int
import Skeletest
import System.ByteOrder
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Axes
import Telescope.Data.Binary
import Telescope.Data.Parser
import Test.Asdf.DecodeSpec (ExampleTreeFix (..), parseIO)


spec :: Spec
spec = do
  -- can we correctly decode an array?
  describe "DataType" $ do
    it "parses Bools" $ do
      let input = [True, False, False, True]
      let arr = toNDArray input
      arr.shape `shouldBe` Axes [length input]
      arr.datatype `shouldBe` Bool8

      res <- parseIO $ fromNDArray arr
      res `shouldBe` [True, False, False, True]

    it "parses ucs4" $ do
      let input :: [Text] = ["one", "two", "three", "four!", "1234567"]
      let arr = toNDArray input
      arr.shape `shouldBe` Axes [length input]
      arr.datatype `shouldBe` Ucs4 7

      res <- parseIO $ fromNDArray arr
      res `shouldBe` input

  it "should parse NDArrayData" $ do
    ExampleTreeFix (Tree tree) <- getFixture
    nd <- expectNDArray $ lookup "sequence" tree
    nd.byteorder `shouldBe` LittleEndian
    nd.datatype `shouldBe` Int64
    nd.shape `shouldBe` axesRowMajor [100]
    BS.length nd.bytes `shouldBe` (100 * byteSize @Int64)

  it "should contain data" $ do
    ExampleTreeFix (Tree tree) <- getFixture
    nd <- expectNDArray $ lookup "sequence" tree
    decodeNums nd.bytes `shouldBe` [0 :: Int64 .. 99]
 where
  decodeNums bytes = do
    runGet (replicateM 100 (get LittleEndian)) (BL.fromStrict bytes) :: [Int64]


expectNDArray :: Maybe Node -> IO NDArrayData
expectNDArray = \case
  Just (Node _ _ (NDArray dat)) -> pure dat
  n -> fail $ "Expected NDArray, but got: " ++ show n
