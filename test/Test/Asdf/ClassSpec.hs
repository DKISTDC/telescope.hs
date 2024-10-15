module Test.Asdf.ClassSpec where

import Conduit
import Data.ByteString (ByteString)
import Data.Massiv.Array (Array, Comp (..), D, Ix1, P)
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Effectful.Resource
import GHC.Generics (Generic, from)
import GHC.Int (Int32, Int64)
import Skeletest
import System.ByteOrder
import Telescope.Asdf.Class
import Telescope.Asdf.Encoding
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Error
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Axes
import Telescope.Data.Parser
import Test.Asdf.DecodeSpec (ExampleTreeFix (..), parseIO)
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
    ex <- parseIO $ parseValue @Example (Object tree)
    ex.foo `shouldBe` 42
    ex.name `shouldBe` "Monty"
    ex.sequence `shouldBe` [0 .. 99]

  it "parses sequence from example.asdf as an NDArray" $ do
    ExampleTreeFix (Tree tree) <- getFixture
    (Sequence s) <- parseIO $ parseValue @Sequence (Object tree)
    s `shouldBe` M.delay (M.fromLists' @P Seq [0 .. 99])

  it "resolves anchors" $ do
    let ex = AnchorExample "world" "asdf"
    out <- runAsdfM $ encode ex
    ex2 <- runAsdfM $ decode @AnchorExample out
    ex2.message `shouldBe` "world"
    ex2.alias `shouldBe` "world"


toAsdfSpec :: Spec
toAsdfSpec = do
  it "should serialize Example" $ do
    let ex = Example{foo = 40, name = "Marty", sequence = [], powers = Nothing, random = M.empty}
    let Node s anc val = toNode ex
    s `shouldBe` schema ex
    anc `shouldBe` Just "example"
    o <- expectObject val
    lookup "foo" o `shouldBe` Just (Node mempty Nothing (Integer 40))
    lookup "name" o `shouldBe` Just (Node mempty Nothing (String "Marty"))

  it "should serialize Example sequence as list" $ do
    let val = toValue $ Example{foo = 40, name = "Marty", sequence = [0 .. 99], powers = Nothing, random = M.empty}
    o <- expectObject val
    ns <- expectArray $ lookup "sequence" o
    ns `shouldBe` fmap (fromValue . Integer) [0 .. 99]

  it "should serialize list to Array" $ do
    let nums = [0 .. 99] :: [Int]
    toValue nums `shouldBe` Array (fmap (Node mempty Nothing . Integer) [0 .. 99])

  it "should forward schema to maybes" $ do
    schema @(Maybe Example) Nothing `shouldBe` mempty
    schema @(Maybe Example) (Just undefined) `shouldBe` schema @Example undefined



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


-- newtype ExampleAsdfFix = ExampleAsdfFix Asdf
-- instance Fixture ExampleAsdfFix where
--   fixtureAction = do
--     ExampleFileFix _ f <- getFixture
--     a <- runAsdfM $ fromAsdfFile f.tree f.blocks
--     pure $ noCleanup $ ExampleAsdfFix a

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
  schema _ = "example/woot-1.0"
  anchor _ = Just "example"
instance FromAsdf Example where
  parseValue = \case
    Object o -> do
      foo <- o .: "foo"
      name <- o .: "name"
      sq <- o .: "sequence" >>= parseSequence
      powers <- o .:? "powers"
      random <- o .: "random"
      pure $ Example{foo, name, sequence = sq, powers, random}
    val -> expected "Example" val
   where
    parseSequence = \case
      Array ns -> mapM parseNode ns
      NDArray nd -> fromNDArray nd
      val -> expected "Array or NDArray" val


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
    node -> expected "Example Sequence" node


data AnchorExample = AnchorExample
  { message :: Text
  , alias :: Text
  }
  deriving (Generic, FromAsdf)
instance ToAsdf AnchorExample where
  toValue ae =
    Object
      [ ("message", Node mempty (Just "message") $ String ae.message)
      , ("alias", toNode $ Alias "message")
      ]


expectArray :: Maybe Node -> IO [Node]
expectArray = \case
  Just (Node _ _ (Array ns)) -> pure ns
  n -> fail $ "Expected Array, but got: " ++ show n


expectObject :: Value -> IO Object
expectObject = \case
  Object o -> pure o
  n -> fail $ "Expected Object, but got: " ++ show n
