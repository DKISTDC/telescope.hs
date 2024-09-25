module Test.Asdf.DecodeSpec where

import Control.Monad.Catch (throwM)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Massiv.Array (Array, D, Ix1)
import Data.Massiv.Array qualified as M
import Data.Text (Text, unpack)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Dynamic
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Encoding
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Error
import Telescope.Asdf.Node
import Telescope.Asdf.Reference
import Telescope.Data.Parser
import Test.Asdf.FileSpec (ExampleFileFix (..))


spec :: Spec
spec = do
  describe "basic" basicSpec
  describe "example" exampleSpec
  describe "dkist" dkistSpec
  describe "references" referenceSpec


basicSpec :: Spec
basicSpec = do
  describe "basic" $ do
    it "should parse asdf" $ do
      ExampleAsdfFix a <- getFixture
      a.library.name `shouldBe` "asdf"
      length a.history.extensions `shouldBe` 1

    it "should parse tree" $ do
      ExampleTreeFix tree <- getFixture
      lookup "foo" tree `shouldBe` Just (Node mempty (Integer 42))
      lookup "name" tree `shouldBe` Just (Node mempty (String "Monty"))

    it "removes asdf_library and history from tree" $ do
      ExampleTreeFix tree <- getFixture
      lookup "asdf_library" tree `shouldSatisfy` P.nothing
      lookup "history" tree `shouldSatisfy` P.nothing


exampleSpec :: Spec
exampleSpec = do
  it "should parse example.asdf" $ do
    inp <- BS.readFile "samples/example.asdf"
    e <- decodeM @Example inp
    e.name `shouldBe` "Monty"
    e.foo `shouldBe` 42
    e.items `shouldBe` ["one", "two", "three", "four", "five"]


data Example = Example
  { foo :: Int
  , name :: Text
  , items :: [Text]
  }
  deriving (Generic, FromAsdf, ToAsdf)


referenceSpec :: Spec
referenceSpec = withMarkers ["focus"] $ do
  it "should parse pointers" $ do
    pointer "/users/1/name" `shouldBe` Pointer (Path [Child "users", Index 1, Child "name"])
    pointer "" `shouldBe` Pointer (Path [])
    pointer "#" `shouldBe` Pointer (Path [])
    pointer "/" `shouldBe` Pointer (Path [])
    pointer "users" `shouldBe` Pointer (Path [Child "users"])
    pointer "/users" `shouldBe` Pointer (Path [Child "users"])
    pointer "#users" `shouldBe` Pointer (Path [Child "users"])

  it "should show pointers" $ do
    let point = Pointer (Path [Child "users", Index 0, Child "name"])
    show point `shouldBe` "#/users/0/name"

  it "should show references" $ do
    let point = Pointer (Path [Child "users", Index 0, Child "name"])
    let uri = "https://example.com/document.asdf/"
    show (Reference uri point) `shouldBe` "https://example.com/document.asdf/#/users/0/name"

  it "should locate pointer" $ do
    RefTreeFix tree <- getFixture
    n0 <- runParse $ runAsdfParser tree $ findPointer (pointer "#/users/0/name")
    n0 `shouldBe` "Monty"

    n1 <- runParse $ runAsdfParser tree $ findPointer (pointer "/users/1/name")
    n1 `shouldBe` "Harold"

  it "parses InternalRef to CurrentUsername with tree" $ do
    RefTreeFix (Tree tree) <- getFixture
    cu <- runParse $ runAsdfParser (Tree tree) $ parseValue @CurrentUsername (InternalRef $ pointer "#/users/2/name")
    cu `shouldBe` CurrentUsername "Sandra"

  it "parses InternalRef to RefResolved with tree" $ do
    RefTreeFix (Tree tree) <- getFixture
    r <- runParse $ runAsdfParser (Tree tree) $ parseValue @RefResolved (Object tree)
    length r.users `shouldBe` 3
    r.currentUsername `shouldBe` CurrentUsername "Harold"

  it "should parse internal references from sample file" $ do
    inp <- BS.readFile "./samples/reference.asdf"
    r <- decodeM @RefResolved inp
    length r.users `shouldBe` 3
    fmap (.name) r.users `shouldBe` ["Monty", "Harold", "Sandra"]
    r.currentUsername `shouldBe` CurrentUsername "Harold"


data RefResolved = RefResolved
  { currentUsername :: CurrentUsername
  , users :: [RefUser]
  }
  deriving (Generic, FromAsdf, Show)


data RefUser = RefUser
  { name :: Text
  }
  deriving (Generic, FromAsdf, Show)


newtype CurrentUsername = CurrentUsername Text
  deriving (Show, Eq)
instance FromAsdf CurrentUsername where
  parseValue = \case
    String s -> pure $ CurrentUsername s
    -- TODO: this should be automagic
    InternalRef p -> parsePointer p
    val -> expected "UsernameRef" val
instance ToAsdf CurrentUsername where
  toValue (CurrentUsername _) = InternalRef $ pointer "/users/2/name"


dkistSpec :: Spec
dkistSpec = do
  it "should parse dkist asdf" $ do
    inp <- BS.readFile "samples/dkist.asdf"
    d <- decodeM @DKISTAsdf inp
    d.dataset.unit `shouldBe` Count
    d.dataset.meta.inventory.datasetId `shouldBe` "AVORO"

    let us = d.dataset.meta.headers.bunit
    take 3 us `shouldBe` ["ct", "ct", "ct"]

    take 3 (M.toLists d.dataset.meta.headers.naxis2) `shouldBe` [998, 998, 998]


data DKISTAsdf = DKISTAsdf
  { dataset :: Dataset
  }
  deriving (Generic, FromAsdf)


data Dataset = Dataset
  { unit :: Unit
  , meta :: Meta
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
  , bitpix :: [Int64]
  , bunit :: [Text]
  }


instance FromAsdf MetaHeaders where
  parseValue = \case
    Object o -> do
      ns <- o .: "columns"
      naxis <- parseColumn "NAXIS" ns
      naxis2 <- parseColumn "NAXIS2" ns
      bitpix <- parseColumn "BITPIX" ns
      bunit <- parseColumn "BUNIT" ns
      pure MetaHeaders{naxis, naxis2, bitpix, bunit}
    val -> expected "Columns" val
   where
    parseColumn :: forall a es. (FromAsdf a, Reader Tree :> es, Parser :> es) => Text -> [Node] -> Eff es a
    parseColumn name ns = do
      case find (isColumnName name) ns of
        Just (Node _ (Object o)) ->
          o .: "data"
        _ -> parseFail $ "Column " ++ unpack name ++ " not found"

    isColumnName n = \case
      Node _ (Object o) -> do
        lookup "name" o == Just (Node mempty (String n))
      _ -> False


newtype ExampleAsdfFix = ExampleAsdfFix Asdf
instance Fixture ExampleAsdfFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    a <- runAsdfM $ fromAsdfFile f.tree f.blocks
    pure $ noCleanup $ ExampleAsdfFix a


newtype ExampleTreeFix = ExampleTreeFix Object
instance Fixture ExampleTreeFix where
  fixtureAction = do
    ExampleAsdfFix a <- getFixture
    let Tree tree = a.tree
    pure $ noCleanup $ ExampleTreeFix tree


newtype RefTreeFix = RefTreeFix Tree
instance Fixture RefTreeFix where
  fixtureAction = do
    let user n = toNode $ Object [("name", toNode (String n))]
    let users = toNode $ Array [user "Monty", user "Harold", user "Sandra"]
    let curr = toNode $ InternalRef $ pointer "#/users/1/name"
    let tree = Tree [("users", users), ("currentUsername", toNode curr)]
    pure $ noCleanup $ RefTreeFix tree


runParse :: Eff '[Error ParseError, IOE] a -> IO a
runParse = runEff . runErrorNoCallStackWith @ParseError throwM
