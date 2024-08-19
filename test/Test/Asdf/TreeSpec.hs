module Test.Asdf.TreeSpec where

import Conduit
import Control.Monad (replicateM)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Effectful
import Effectful.Fail
import Effectful.Resource
import GHC.Int (Int32, Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import System.ByteOrder
import Telescope.Asdf.Class
import Telescope.Asdf.Document
import Telescope.Asdf.Encoding
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Telescope.Data.Axes
import Telescope.Data.Binary (BinaryValue (..))
import Text.Libyaml qualified as Yaml


spec :: Spec
spec = do
  describe "tree" $ do
    describe "basic" $ do
      it "should parse some fields" $ do
        ExampleTreeFix obj <- getFixture
        lookup "foo" obj `shouldBe` Just (Node mempty (Integer 42))
        lookup "name" obj `shouldBe` Just (Node mempty (String "Monty"))

    describe "NDArray" $ do
      it "should parse NDArrayData" $ do
        ExampleTreeFix obj <- getFixture
        nd <- expectNDArray $ lookup "sequence" obj
        nd.byteorder `shouldBe` LittleEndian
        nd.datatype `shouldBe` Int64
        nd.shape `shouldBe` axesRowMajor [100]
        BS.length nd.bytes `shouldBe` (100 * byteSize @Int64)

      it "should contain data" $ do
        ExampleTreeFix obj <- getFixture
        nd <- expectNDArray $ lookup "sequence" obj
        decodeNums nd.bytes `shouldBe` [0 :: Int64 .. 99]

    describe "FromAsdf" $ do
      it "FromAsdf Example" $ do
        ExampleTreeFix obj <- getFixture
        case runParser $ parseValue @Example (Object obj) of
          Left e -> fail e
          Right ex -> do
            ex.foo `shouldBe` 42
            ex.name `shouldBe` "Monty"
            ex.sequence `shouldBe` [0 .. 99]

    describe "ToAsdf" $ do
      it "should serialize" $ do
        let ex = Example{foo = 40, name = "Marty", sequence = [10, 20, 30, 40]}
        let Node s val = toNode ex
        s `shouldBe` schema @Example
        o <- expectObject val
        lookup "foo" o `shouldBe` Just (Node mempty (Integer 40))
        lookup "name" o `shouldBe` Just (Node mempty (String "Marty"))
        ns <- expectArray $ lookup "sequence" o
        fmap (.value) ns `shouldBe` fmap (Integer . fromIntegral) ex.sequence

      it "should serialize long sequence to NDArray" $ do
        let ex = Example{foo = 40, name = "Marty", sequence = map (* 10) [0 .. 99]}
        let Node _ val = toNode ex
        o <- expectObject val
        dat <- expectNDArray $ lookup "sequence" o
        BS.length dat.bytes `shouldSatisfy` P.gt 0
        dat.datatype `shouldBe` Int64
 where
  -- Redundant
  -- it "should decode squares" $ do
  --   ExampleTreeFix obj <- getFixture
  --   Just (Node _ (Object pw)) <- pure $ lookup "powers" obj
  --   nd <- expectNDArray $ lookup "squares" pw
  --   decodeNums nd.bytes `shouldBe` map (^ 2) [0 :: Int64 .. 99]

  expectNDArray = \case
    Just (Node _ (NDArray dat)) -> pure dat
    n -> fail $ "Expected NDArray, but got: " ++ show n

  expectArray = \case
    Just (Node _ (Array ns)) -> pure ns
    n -> fail $ "Expected Array, but got: " ++ show n

  expectObject = \case
    Object o -> pure o
    n -> fail $ "Expected Object, but got: " ++ show n

  decodeNums bytes = do
    runGet (replicateM 100 (get LittleEndian)) (BL.fromStrict bytes) :: [Int64]


newtype ExamplePartsFix = ExamplePartsFix DocumentParts
instance Fixture ExamplePartsFix where
  fixtureAction = do
    inp <- BS.readFile "samples/example.asdf"
    dp <- runEff $ runFailIO $ splitDocument inp
    pure $ noCleanup $ ExamplePartsFix dp


newtype ExampleTreeFix = ExampleTreeFix Object
instance Fixture ExampleTreeFix where
  fixtureAction = do
    ExamplePartsFix dp <- getFixture
    node <- runEff $ runFailIO $ parseTree dp.tree dp.blocks
    case node of
      (Node _ (Object o)) -> pure $ noCleanup $ ExampleTreeFix o
      _ -> fail $ "Expected top-level Object, but got " ++ show node


dumpEvents :: ByteString -> IO ()
dumpEvents inp = do
  runEff $ runFailIO $ do
    dp <- splitDocument inp
    runResource $ runConduit $ Yaml.decode dp.tree .| takeC 100 .| mapM_C (liftIO . print)


data Example = Example
  { foo :: Int32
  , name :: Text
  , sequence :: [Int64]
  }


instance FromAsdf Example where
  parseValue = \case
    Object o -> do
      foo <- o .: "foo"
      name <- o .: "name"
      sq <- o .: "sequence"
      pure Example{foo, name, sequence = sq}
    node -> fail $ expected "Example Object" node


instance ToAsdf Example where
  schema = "example/woot-1.0"
  toValue ex =
    Object
      [ ("foo", toNode ex.foo)
      , ("name", toNode ex.name)
      , ("sequence", toNode ex.sequence)
      ]
