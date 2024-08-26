module Test.Asdf.ClassSpec where

import Conduit
import Data.ByteString (ByteString)
import Data.Massiv.Array (Array, Comp (..), D, Ix1, P)
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.Resource
import GHC.Generics (Generic, from)
import GHC.Int (Int32, Int64)
import Skeletest
import System.ByteOrder
import Telescope.Asdf.Class
import Telescope.Asdf.Core (Asdf (..))
import Telescope.Asdf.Encoding
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Error
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Asdf.Parser
import Telescope.Data.Axes
import Test.Asdf.FileSpec (ExampleFileFix (..))
import Text.Libyaml qualified as Yaml


spec :: Spec
spec = do
  describe "FromAsdf" fromAsdfSpec
  describe "ToAsdf" toAsdfSpec
  describe "GObject" gObjectSpec


fromAsdfSpec :: Spec
fromAsdfSpec = do
  it "parses object" $ do
    let (tree :: Object) =
          [ ("foo", fromValue $ Integer 42)
          , ("name", fromValue $ String "Monty")
          , ("sequence", fromValue $ Array $ fmap (fromValue . Integer) [0 .. 99])
          , ("random", fromValue $ NDArray $ NDArrayData "" BigEndian Float64 (axesRowMajor [0]))
          ]
    case runParser $ parseValue @Example (Object tree) of
      Left e -> fail e
      Right ex -> do
        ex.foo `shouldBe` 42
        ex.name `shouldBe` "Monty"
        ex.sequence `shouldBe` [0 .. 99]

  it "parses sequence from example.asdf as an NDArray" $ do
    ExampleAsdfFix a <- getFixture
    case runParser $ parseValue @Sequence (Object a.tree) of
      Left e -> fail e
      Right (Sequence s) -> do
        s `shouldBe` M.delay (M.fromLists' @P Seq [0 .. 99])


toAsdfSpec :: Spec
toAsdfSpec = do
  it "should serialize Example" $ do
    let Node s val = toNode $ Example{foo = 40, name = "Marty", sequence = [], powers = Nothing, random = M.empty}
    s `shouldBe` schema @Example
    o <- expectObject val
    lookup "foo" o `shouldBe` Just (Node mempty (Integer 40))
    lookup "name" o `shouldBe` Just (Node mempty (String "Marty"))

  it "should serialize Example sequence as list" $ do
    let val = toValue $ Example{foo = 40, name = "Marty", sequence = [0 .. 99], powers = Nothing, random = M.empty}
    o <- expectObject val
    ns <- expectArray $ lookup "sequence" o
    ns `shouldBe` fmap (fromValue . Integer) [0 .. 99]

  it "should serialize list to Array" $ do
    let nums = [0 .. 99] :: [Int]
    toValue nums `shouldBe` Array (fmap (Node mempty . Integer) [0 .. 99])


-- it "should produce similar example.asdf" $ do
--   ExampleFileFix inp _ <- getFixture
--   e <- decodeM @Example inp
--   -- o <- encodeM e
--   -- BS.writeFile "dump/test.asdf" o
--   -- print e
--   fail ":NOPE"

gObjectSpec :: Spec
gObjectSpec = do
  it "should gen object" $ do
    gToObject (from (TinyGen "world")) `shouldBe` [("hello", fromValue (String "world"))]

  it "should gen tiny type" $ do
    toValue (TinyGen "world") `shouldBe` Object [("hello", fromValue (String "world"))]

  it "should gen maybe type" $ do
    toValue (MaybeGen (Just "world")) `shouldBe` Object [("hello", fromValue (String "world"))]
    toValue (MaybeGen Nothing) `shouldBe` Object [("hello", fromValue Null)]

  it "should allow maybes" $ do
    let val = Object [("hello", fromValue (String "world"))]
    m1 <- parseIO (parseValue val)
    m1 `shouldBe` MaybeGen (Just "world")

    m2 <- parseIO (parseValue $ Object [])
    m2 `shouldBe` MaybeGen Nothing


newtype ExampleAsdfFix = ExampleAsdfFix Asdf
instance Fixture ExampleAsdfFix where
  fixtureAction = do
    ExampleFileFix _ f <- getFixture
    a <- runAsdfM $ fromAsdfFile f.tree f.blocks
    pure $ noCleanup $ ExampleAsdfFix a


dumpEvents :: ByteString -> IO ()
dumpEvents inp = do
  runAsdfM $ do
    a <- splitAsdfFile inp
    runResource $ runConduit $ Yaml.decode a.tree.bytes .| takeC 100 .| mapM_C (liftIO . print)


data TinyGen = TinyGen
  { hello :: Text
  }
  deriving (Generic, FromAsdf, ToAsdf)


data MaybeGen = MaybeGen
  { hello :: Maybe Text
  }
  deriving (Generic, FromAsdf, ToAsdf, Eq)


data Example = Example
  { foo :: Int32
  , name :: Text
  , powers :: Maybe Powers
  , sequence :: [Int64]
  , random :: Array D Ix1 Double
  }
  deriving (Generic, Show)
instance ToAsdf Example where
  schema = "example/woot-1.0"
instance FromAsdf Example where
  parseValue = \case
    Object o -> do
      foo <- o .: "foo"
      name <- o .: "name"
      sq <- o .: "sequence" >>= parseSequence
      powers <- o .:? "powers"
      random <- o .: "random"
      pure $ Example{foo, name, sequence = sq, powers, random}
    val -> fail $ expected "Example" val
   where
    parseSequence = \case
      Array ns -> mapM parseNode ns
      NDArray nd -> fromNDArray nd
      val -> fail $ expected "Array or NDArray" val


data Powers = Powers
  {squares :: Array D Ix1 Int64}
  deriving (Generic, Show, ToAsdf, FromAsdf)


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


parseIO :: Parser a -> IO a
parseIO p = runEff $ runErrorNoCallStackWith @ParseError throwM $ fromParser p
