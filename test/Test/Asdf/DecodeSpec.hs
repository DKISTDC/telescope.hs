module Test.Asdf.DecodeSpec where

import Conduit
import Control.Monad (replicateM)
import Data.Binary.Get (runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import System.ByteOrder
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Decoding
import Telescope.Asdf.Error
import Telescope.Asdf.File
import Telescope.Asdf.Node
import Telescope.Data.Axes
import Telescope.Data.Binary (BinaryValue (..))
import Test.Asdf.FileSpec (ExampleFileFix (..))


spec :: Spec
spec = do
  describe "basic" basicSpec
  describe "NDArray" ndArraySpec
  describe "example" exampleSpec


basicSpec :: Spec
basicSpec = do
  describe "basic" $ do
    it "should parse asdf" $ do
      ExampleAsdfFix a <- getFixture
      a.library.name `shouldBe` "asdf"
      length a.history.extensions `shouldBe` 1

    it "should parse tree" $ do
      ExampleAsdfFix a <- getFixture
      lookup "foo" a.tree `shouldBe` Just (Node mempty (Integer 42))
      lookup "name" a.tree `shouldBe` Just (Node mempty (String "Monty"))

    it "removes asdf_library and history from tree" $ do
      ExampleAsdfFix a <- getFixture
      lookup "asdf_library" a.tree `shouldSatisfy` P.nothing
      lookup "history" a.tree `shouldSatisfy` P.nothing


ndArraySpec :: Spec
ndArraySpec = do
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


exampleSpec :: Spec
exampleSpec = do
  it "should parse example.asdf" $ do
    inp <- BS.readFile "samples/example.asdf"
    e <- decodeM @Example inp
    e.name `shouldBe` "Monty"
    e.foo `shouldBe` 42


data Example = Example
  { foo :: Int
  , name :: Text
  }
  deriving (Generic, FromAsdf)


newtype ExampleAsdfFix = ExampleAsdfFix Asdf
instance Fixture ExampleAsdfFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    a <- runEff $ runErrorNoCallStackWith @AsdfError throwM $ fromAsdfFile f.tree f.blocks
    pure $ noCleanup $ ExampleAsdfFix a


expectNDArray :: Maybe Node -> IO NDArrayData
expectNDArray = \case
  Just (Node _ (NDArray dat)) -> pure dat
  n -> fail $ "Expected NDArray, but got: " ++ show n
