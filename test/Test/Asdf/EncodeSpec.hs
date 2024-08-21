module Test.Asdf.EncodeSpec where

import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Massiv.Array (Array, Comp (Seq), D, Ix2, P)
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import GHC.Generics (Generic)
import GHC.Int (Int16, Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Decoding
import Telescope.Asdf.Encoding
import Telescope.Asdf.Error
import Telescope.Asdf.File
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Test.Asdf.ClassSpec (Example (..), expectObject)


spec :: Spec
spec = do
  describe "basic" basicSpec
  describe "document" documentSpec
  describe "blocks" blocksSpec
  describe "roundtrip" roundSpec


basicSpec :: Spec
basicSpec = do
  it "starts with required header lines" $ do
    out <- encodeM (Object [])
    (l1 : l2 : l3 : l4 : doc1 : _) <- pure $ BC.lines out
    l1 `shouldBe` "#ASDF 1.0.0"
    l2 `shouldBe` "#ASDF_STANDARD 1.5.0"
    l3 `shouldBe` "%YAML 1.1"
    l4 `shouldBe` "%TAG ! tag:stsci.edu:asdf/"
    BS.take 4 doc1 `shouldBe` "--- "

  it "should include history" $ do
    out <- encodeM (Object [])
    let (_, restL) = BS.breakSubstring "asdf_library:" out
    BS.length restL `shouldNotBe` 0
    let (_, restH) = BS.breakSubstring "history:" out
    BS.length restH `shouldNotBe` 0

  it "should throw if not an object" $ do
    encodeM (Integer 100) `shouldSatisfy` P.throws @AsdfError P.anything


documentSpec :: Spec
documentSpec = do
  it "converts to document" $ do
    asdf <- runEff . runErrorNoCallStackWith @AsdfError throwM $ toDocument $ BasicData "henry"
    asdf.library `shouldBe` telescopeSoftware
    lookup "username" asdf.tree `shouldBe` Just "henry"


blocksSpec :: Spec
blocksSpec = do
  it "includes blocks" $ do
    let ns = [1 .. 100]
    out <- encodeM (BasicArray ns)
    af <- runEff . runErrorNoCallStackWith @AsdfError throwM $ splitAsdfFile out
    length af.blocks `shouldBe` 1

    [BlockData bd] <- pure af.blocks
    bd `shouldBe` (toNDArray ns).bytes


roundSpec :: Spec
roundSpec = do
  it "decodes encoded file" $ do
    out <- encodeM (Object [("hello", "world")])
    tree <- decodeM @Value out >>= expectObject
    lookup "hello" tree `shouldSatisfy` P.just (P.eq "world")

  it "encodes data type fields" $ do
    out <- encodeM $ BasicData "hello"
    let (_, nameRest) = BS.breakSubstring "username: hello" out
    nameRest `shouldNotBe` ""

  it "encodes simple ndarray" $ do
    out <- encodeM $ BasicArray [1 .. 100]
    BasicArray ns <- decodeM out
    ns `shouldBe` [1 .. 100]

  it "encodes massiv array" $ do
    let mx = matrix [[1.0 .. 5.0], [2.0 .. 6.0]]
    out <- encodeM $ Matrix mx

    -- TODO: we should throw an error if the shapes don't match
    -- TEST: throws an error if NDArrayData.shape doesn't match
    Matrix ns <- decodeM out
    ns `shouldBe` mx

  it "decodes encoded data type" $ do
    let sd = SomeData 24 ["one", "two"] $ matrix [[1, 2, 3], [4, 5, 6]]
    out <- encodeM sd
    sd2 <- decodeM @SomeData out
    sd2.number `shouldBe` sd.number
    sd2.tags `shouldBe` sd.tags
    sd2.matrix `shouldBe` sd.matrix
 where
  matrix ns = M.delay @Ix2 @P $ M.fromLists' Seq ns


newtype BasicArray = BasicArray [Int64]
instance ToAsdf BasicArray where
  toValue (BasicArray ns) =
    Object
      [("values", fromValue $ NDArray $ toNDArray ns)]
instance FromAsdf BasicArray where
  parseValue = \case
    Object o -> do
      nd <- o .: "values"
      ns <- fromNDArray nd
      pure $ BasicArray ns
    val -> fail $ expected "BasicArray.values" val


data Matrix = Matrix
  { values :: Array D Ix2 Double
  }
  deriving (Generic, ToAsdf, FromAsdf)


data BasicData = BasicData
  { username :: Text
  }
  deriving (Generic, ToAsdf, FromAsdf)


data SomeData = SomeData
  { number :: Int
  , tags :: [Text]
  , matrix :: Array D Ix2 Int16
  }
  deriving (Generic, ToAsdf, FromAsdf)

-- TEST: round trip file parts
-- TEST: round trip is the only good way
