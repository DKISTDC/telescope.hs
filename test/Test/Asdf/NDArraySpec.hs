module Test.Asdf.NDArraySpec where

import Control.Monad (replicateM)
import Data.Binary.Get (runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import GHC.Int
import Skeletest
import System.ByteOrder
import Telescope.Asdf.Core
import Telescope.Asdf.Node
import Telescope.Data.Axes
import Telescope.Data.Binary
import Test.Asdf.DecodeSpec (ExampleAsdfFix (..))


spec :: Spec
spec = do
  -- can we correctly decode an array?
  describe "DataType" $ do
    -- TODO: create smaller DKIST Asdf via python notebook
    -- TEST: parse bool8
    -- TEST: parse uc4
    pure ()

  it "should parse NDArrayData" $ do
    ExampleAsdfFix a <- getFixture
    nd <- expectNDArray $ lookup "sequence" a.tree
    nd.byteorder `shouldBe` LittleEndian
    nd.datatype `shouldBe` Int64
    nd.shape `shouldBe` axesRowMajor [100]
    BS.length nd.bytes `shouldBe` (100 * byteSize @Int64)

  it "should contain data" $ do
    ExampleAsdfFix a <- getFixture
    nd <- expectNDArray $ lookup "sequence" a.tree
    decodeNums nd.bytes `shouldBe` [0 :: Int64 .. 99]
 where
  decodeNums bytes = do
    runGet (replicateM 100 (get LittleEndian)) (BL.fromStrict bytes) :: [Int64]


expectNDArray :: Maybe Node -> IO NDArrayData
expectNDArray = \case
  Just (Node _ (NDArray dat)) -> pure dat
  n -> fail $ "Expected NDArray, but got: " ++ show n
