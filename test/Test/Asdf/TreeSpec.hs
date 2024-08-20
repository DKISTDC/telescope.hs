module Test.Asdf.TreeSpec where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Array, Comp (..), D, Ix1, P)
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.Resource
import GHC.Int (Int32, Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core (Asdf (..))
import Telescope.Asdf.Decoding
import Telescope.Asdf.Error
import Telescope.Asdf.File
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Test.Asdf.DecodeSpec (expectNDArray)
import Test.Asdf.FileSpec (ExampleFileFix (..))
import Text.Libyaml qualified as Yaml


spec :: Spec
spec = do
  describe "FromAsdf" fromAsdfSpec
  describe "ToAsdf" toAsdfSpec


fromAsdfSpec :: Spec
fromAsdfSpec = do
  it "parses object" $ do
    let (tree :: Object) =
          [ ("foo", fromValue $ Integer 42)
          , ("name", fromValue $ String "Monty")
          , ("sequence", fromValue $ Array $ fmap (fromValue . Integer) [0 .. 99])
          ]
    case runParser $ parseValue @ExampleList (Object tree) of
      Left e -> fail e
      Right ex -> do
        ex.foo `shouldBe` 42
        ex.name `shouldBe` "Monty"
        ex.sequence `shouldBe` [0 .. 99]

  it "parses sequence from example.asdf as an Array" $ do
    ExampleAsdfFix a <- getFixture
    print a.tree
    case runParser $ parseValue @Sequence (Object a.tree) of
      Left e -> fail e
      Right (Sequence s) -> do
        s `shouldBe` M.delay (M.fromLists' @P Seq [0 .. 99])


toAsdfSpec :: Spec
toAsdfSpec = do
  it "should serialize data type" $ do
    let Node s val = toNode $ ExampleList{foo = 40, name = "Marty", sequence = []}
    s `shouldBe` schema @ExampleList
    o <- expectObject val
    lookup "foo" o `shouldBe` Just (Node mempty (Integer 40))
    lookup "name" o `shouldBe` Just (Node mempty (String "Marty"))
    ns <- expectArray $ lookup "sequence" o
    fmap (.value) ns `shouldBe` []

  it "should serialize list to Array" $ do
    let nums = [0 .. 99] :: [Int]
    toValue nums `shouldBe` Array (fmap (Node mempty . Integer) [0 .. 99])


-- it "should serialize Array to NDArray" $ do
--   let n = toNode ([1 .. 100] :: [Int])
--   as <- expectArray (Just n)
--   as `shouldBe` fmap (Node mempty . Integer) [1 .. 100]

newtype ExampleAsdfFix = ExampleAsdfFix Asdf
instance Fixture ExampleAsdfFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    a <- runEff $ runErrorNoCallStackWith @AsdfError throwM $ fromAsdfFile f.tree f.blocks
    pure $ noCleanup $ ExampleAsdfFix a


dumpEvents :: ByteString -> IO ()
dumpEvents inp = do
  runEff $ runErrorNoCallStackWith @AsdfError throwM $ do
    a <- splitAsdfFile inp
    runResource $ runConduit $ Yaml.decode a.tree .| takeC 100 .| mapM_C (liftIO . print)


data ExampleList = ExampleList
  { foo :: Int32
  , name :: Text
  , sequence :: [Int64]
  }


instance FromAsdf ExampleList where
  parseValue = \case
    Object o -> do
      foo <- o .: "foo"
      name <- o .: "name"
      sql <- o .: "sequence"
      pure ExampleList{foo, name, sequence = sql}
    node -> fail $ expected "Example Object" node


instance ToAsdf ExampleList where
  schema = "example/woot-1.0"
  toValue ex =
    Object
      [ ("foo", toNode ex.foo)
      , ("name", toNode ex.name)
      , ("sequence", toNode ex.sequence)
      ]


data Sequence = Sequence (Array D Ix1 Int64)
  deriving (Eq)
instance FromAsdf Sequence where
  parseValue = \case
    Object o -> do
      sql <- o .: "sequence"
      pure $ Sequence sql
    node -> fail $ expected "Example Sequence" node


expectArray :: Maybe Node -> IO [Node]
expectArray = \case
  Just (Node _ (Array ns)) -> pure ns
  n -> fail $ "Expected Array, but got: " ++ show n


expectObject :: Value -> IO Object
expectObject = \case
  Object o -> pure o
  n -> fail $ "Expected Object, but got: " ++ show n
