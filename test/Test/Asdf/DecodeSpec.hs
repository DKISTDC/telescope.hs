module Test.Asdf.DecodeSpec where

import Control.Monad.Catch (throwM)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Massiv.Array (Array, D, Ix1)
import Data.Massiv.Array qualified as M
import Data.Text (Text, unpack)
import Effectful
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
  describe "anchors" anchorSpec


basicSpec :: Spec
basicSpec = do
  describe "basic" $ do
    it "should parse asdf" $ do
      ExampleTreeFix tree <- getFixture
      a :: Asdf <- runAsdfM $ decodeFromTree tree
      a.library.name `shouldBe` "asdf"
      length a.history.extensions `shouldBe` 1

    it "should parse tree" $ do
      ExampleTreeFix (Tree tree) <- getFixture
      lookup "foo" tree `shouldBe` Just (Node mempty Nothing (Integer 42))
      lookup "name" tree `shouldBe` Just (Node mempty Nothing (String "Monty"))


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


anchorSpec :: Spec
anchorSpec = do
  it "should create anchors" $ do
    let Encoded out = encodeTree "{hello: &hello world}"
    f <- runAsdfM $ splitAsdfFile out
    (_, ancs) <- runAsdfM $ streamAsdfFile f.tree f.blocks
    ancs `shouldBe` Anchors [("hello", String "world")]

  it "should throw missing anchors" $ do
    f <- runAsdfM $ do
      let tree = encodeTree "{message: *somealias}"
      pure $ AsdfFile tree mempty ""
    runAsdfM (streamAsdfFile f.tree f.blocks) `shouldSatisfy` P.throws @AsdfError P.anything

  it "should throw if alias before anchor" $ do
    let Encoded out = encodeTree "{message: *hello, hello: &hello world}"
    decodeM @Tree out `shouldSatisfy` P.throws @AsdfError P.anything

  it "should succeed if anchor before alias" $ do
    let Encoded out = encodeTree "{hello: &hello world, message: *hello}"
    Tree tree <- decodeM @Tree out
    lookup "message" tree `shouldBe` Just "world"

  it "should decode anchors roundtrip" $ do
    let root = [("hello", Node mempty (Just "hello") (String "world")), ("message", toNode $ Alias "hello")] :: Object
    out <- runAsdfM $ encode (Object root)
    Tree tree <- decodeM @Tree out
    lookup "message" tree `shouldBe` Just "world"


referenceSpec :: Spec
referenceSpec = do
  it "should parse pointers" $ do
    jsonPointer "/users/1/name" `shouldBe` JSONPointer (Path [Child "users", Index 1, Child "name"])
    jsonPointer "" `shouldBe` JSONPointer (Path [])
    jsonPointer "#" `shouldBe` JSONPointer (Path [])
    jsonPointer "/" `shouldBe` JSONPointer (Path [])
    jsonPointer "users" `shouldBe` JSONPointer (Path [Child "users"])
    jsonPointer "/users" `shouldBe` JSONPointer (Path [Child "users"])
    jsonPointer "#users" `shouldBe` JSONPointer (Path [Child "users"])

  it "should parse an internal pointer as a reference" $ do
    jsonReference "#/users/1/name" `shouldBe` JSONReference mempty (jsonPointer "#/users/1/name")

  it "should parse an external reference" $ do
    let url = "https://woot.com/asdf"
    let point = "#/users/1/name"
    jsonReference (url <> point) `shouldBe` JSONReference url (jsonPointer point)

  it "should show pointers" $ do
    let point = JSONPointer (Path [Child "users", Index 0, Child "name"])
    show point `shouldBe` "#/users/0/name"

  it "should show references" $ do
    let point = JSONPointer (Path [Child "users", Index 0, Child "name"])
    let uri = "https://example.com/document.asdf/"
    show (JSONReference uri point) `shouldBe` "https://example.com/document.asdf/#/users/0/name"

  it "should locate pointer" $ do
    RefTreeFix tree <- getFixture
    n0 <- parseIO $ findPointer (jsonPointer "#/users/0/name") tree
    n0 `shouldBe` "Monty"

    n1 <- parseIO $ findPointer (jsonPointer "/users/1/name") tree
    n1 `shouldBe` "Harold"


-- I don't think we should automatically resolve any internal references. Assume all references are external
-- it "parses Internal Ref to CurrentUsername with tree" $ do
--   RefTreeFix (Tree tree) <- getFixture
--   cu <- runParse $ runAsdfParser (Tree tree) $ parseValue @CurrentUsername (InternalRef $ pointer "#/users/2/name")
--   cu `shouldBe` CurrentUsername "Sandra"
--
-- it "parses Internal Ref to RefResolved with tree" $ do
--   RefTreeFix (Tree tree) <- getFixture
--   r <- runParse $ runAsdfParser (Tree tree) $ parseValue @RefResolved (Object tree)
--   length r.users `shouldBe` 3
--   r.currentUsername `shouldBe` CurrentUsername "Harold"
--
-- it "should parse internal references from sample file" $ do
--   inp <- BS.readFile "./samples/reference.asdf"
--   r <- decodeM @RefResolved inp
--   length r.users `shouldBe` 3
--   fmap (.name) r.users `shouldBe` ["Monty", "Harold", "Sandra"]
--   r.currentUsername `shouldBe` CurrentUsername "Harold"

-- data RefResolved = RefResolved
--   { currentUsername :: CurrentUsername
--   , users :: [RefUser]
--   }
--   deriving (Generic, FromAsdf, Show)
--
--
-- data RefUser = RefUser
--   { name :: Text
--   }
--   deriving (Generic, FromAsdf, Show)

-- newtype CurrentUsername = CurrentUsername Text
--   deriving (Show, Eq)
-- instance FromAsdf CurrentUsername where
--   parseValue = \case
--     String s -> pure $ CurrentUsername s
--     -- TODO: this should be automagic
--     InternalRef p -> parsePointer p
--     val -> expected "UsernameRef" val
-- instance ToAsdf CurrentUsername where
--   toValue (CurrentUsername _) = InternalRef $ pointer "/users/2/name"

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
    parseColumn :: forall a es. (FromAsdf a, Parser :> es) => Text -> [Node] -> Eff es a
    parseColumn name ns = do
      case find (isColumnName name) ns of
        Just (Node _ _ (Object o)) ->
          o .: "data"
        _ -> parseFail $ "Column " ++ unpack name ++ " not found"

    isColumnName n = \case
      Node _ _ (Object o) -> do
        lookup "name" o == Just (Node mempty Nothing (String n))
      _ -> False


newtype ExampleTreeFix = ExampleTreeFix Tree
instance Fixture ExampleTreeFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    tree <- runAsdfM $ parseAsdfTree f.tree f.blocks
    pure $ noCleanup $ ExampleTreeFix tree


newtype RefTreeFix = RefTreeFix Tree
instance Fixture RefTreeFix where
  fixtureAction = do
    let user n = toNode $ Object [("name", toNode (String n))]
    let users = toNode $ Array [user "Monty", user "Harold", user "Sandra"]
    let curr = toNode $ Reference $ JSONReference mempty (jsonPointer "#/users/1/name")
    let tree = Tree [("users", users), ("currentUsername", toNode curr)]
    pure $ noCleanup $ RefTreeFix tree


parseIO :: Eff '[Parser, IOE] a -> IO a
parseIO p = do
  res <- runEff $ runParser p
  case res of
    Left e -> throwM e
    Right a -> pure a
