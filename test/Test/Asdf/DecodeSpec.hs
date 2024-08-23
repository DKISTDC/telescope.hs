module Test.Asdf.DecodeSpec where

import Conduit
import Control.Monad (replicateM)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Massiv.Array (Array, D, Ix1)
import Data.Massiv.Array qualified as M
import Data.Text (Text, unpack)
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Decoding
import Telescope.Asdf.Error
import Telescope.Asdf.File
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Telescope.Data.Axes
import Test.Asdf.FileSpec (ExampleFileFix (..))


spec :: Spec
spec = do
  describe "basic" basicSpec
  describe "example" exampleSpec
  describe "dkist" dkistSpec


data DKISTAsdf = DKISTAsdf
  { dataset :: Dataset
  }
  deriving (Generic, FromAsdf)


data Dataset = Dataset
  { unit :: Unit
  , meta :: Meta
  -- , _data :: DatasetData
  -- , wcs :: WCS
  }
  deriving (Generic, FromAsdf)


data Meta = Meta
  { headers :: MetaHeaders
  , inventory :: MetaInventory
  }
  deriving (Generic, FromAsdf)


data MetaInventory = MetaInventory
  { bucket :: Text
  , datasetId :: Text
  }
  deriving (Generic, FromAsdf)


-- can we make this work with a generic?
data MetaHeaders = MetaHeaders
  { naxis :: Array D Ix1 Int64
  , naxis2 :: Array D Ix1 Int64
  , bitpix :: Array D Ix1 Int64
  , bunit :: BUnits
  }


newtype BUnits = BUnits [Text]
  deriving (Show)


instance FromAsdf BUnits where
  parseValue = \case
    NDArray dat -> fromNDArray dat
    val -> fail $ expected "NDArray BUnits" val
instance FromNDArray BUnits where
  fromNDArray dat = do
    Axes [num] <- pure dat.shape
    Ucs4 n <- pure dat.datatype
    BUnits <$> parseGet (replicateM num (getUnit n)) dat.bytes
   where
    getUnit n = do
      bs <- getByteString (fromIntegral $ n * 4)
      pure $ T.decodeUtf32LE bs


-- T.decodeUtf32LEWith T.strictDecode

instance FromAsdf MetaHeaders where
  parseValue = \case
    Object o -> do
      Array ns <- o .: "columns"
      naxis <- parseColumn "NAXIS" ns
      naxis2 <- parseColumn "NAXIS2" ns
      bitpix <- parseColumn "BITPIX" ns
      bunit <- parseColumn "BUNIT" ns
      pure MetaHeaders{naxis, naxis2, bitpix, bunit}
    val -> fail $ expected "Columns" val
   where
    parseColumn :: forall a. (FromAsdf a) => Text -> [Node] -> Parser a
    parseColumn name ns = do
      case find (isColumnName name) ns of
        Just (Node _ (Object o)) -> do
          o .: "data"
        _ -> fail $ "Column " ++ unpack name ++ " not found"

    isColumnName n = \case
      Node _ (Object o) -> do
        lookup "name" o == Just (Node mempty (String n))
      _ -> False


dkistSpec :: Spec
dkistSpec = do
  it "should parse dkist asdf" $ do
    inp <- BS.readFile "samples/dkist.asdf"
    d <- decodeM @DKISTAsdf inp
    d.dataset.unit `shouldBe` Count
    d.dataset.meta.inventory.datasetId `shouldBe` "AVORO"

    let BUnits us = d.dataset.meta.headers.bunit
    take 3 us `shouldBe` ["ct", "ct", "ct"]

    take 3 (M.toLists d.dataset.meta.headers.naxis2) `shouldBe` [998, 998, 998]


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
  deriving (Generic, FromAsdf, ToAsdf)


newtype ExampleAsdfFix = ExampleAsdfFix Asdf
instance Fixture ExampleAsdfFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    a <- runAsdfM $ fromAsdfFile f.tree f.blocks
    pure $ noCleanup $ ExampleAsdfFix a
