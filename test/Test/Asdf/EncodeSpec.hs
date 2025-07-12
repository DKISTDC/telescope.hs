module Test.Asdf.EncodeSpec where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Massiv.Array (Array, Comp (Seq), D, Ix2, P)
import Data.Massiv.Array qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GHC.Int (Int16, Int64)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Encoding
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Error
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Parser (expected)
import Test.Asdf.ClassSpec (expectObject)


spec :: Spec
spec = do
  describe "basic" basicSpec
  describe "document" documentSpec
  describe "blocks" blocksSpec
  describe "roundtrip" roundSpec
  describe "stream" streamSpec
  describe "external verification" externalSpec
  describe "references" referenceSpec
  describe "anchors" anchorSpec
  describe "schemas" schemaSpec


schemaSpec :: Spec
schemaSpec = withMarkers ["focus"] $ do
  it "unit nodes have correct schema" $ do
    toNode Nanometers `shouldBe` Node "!unit/unit-1.0.0" Nothing (String "nm")

  it "encoded unit has correct schema" $ do
    (out, _) <- runAsdfM $ encodeNode (toNode Nanometers)
    out `shouldBe` "!unit/unit-1.0.0 nm\n"

  it "encoded quantity has correct schema" $ do
    let q :: Quantity = Quantity Nanometers (Number 854.2)
    (out, _) <- runAsdfM $ encodeNode $ toNode q
    out `shouldBe` "!unit/quantity-1.2.0 {unit: !unit/unit-1.0.0 nm, value: 854.2}\n"


anchorSpec :: Spec
anchorSpec = do
  it "should encode an anchor" $ do
    (out, _) <- runAsdfM . encodeNode $ Node mempty (Just "woot") "hello"
    out `shouldBe` "&woot 'hello'\n"

  it "should encode an alias" $ do
    let alias = toNode $ Alias $ Anchor "something" :: Node
    let anc = Node mempty (Just "something") "something"
    (out, _) <- runAsdfM . encodeNode $ toNode $ Array [anc, alias]
    out `shouldBe` "[&something 'something', *something]\n"

  it "should not encode anchors to mappings" $ do
    (out, _) <- runAsdfM . encodeNode $ Node mempty (Just "thing") (Object [("hello", "world")])
    let outt = T.decodeUtf8 out
    length (T.splitOn "&thing" outt) `shouldBe` 2
    out `shouldBe` "&thing {hello: world}\n"

  it "should not encode anchors to array members" $ do
    (out, _) <- runAsdfM . encodeNode $ Node mempty (Just "thing") (Array ["one", "two"])
    let outt = T.decodeUtf8 out
    length (T.splitOn "&thing" outt) `shouldBe` 2
    out `shouldBe` "&thing [one, two]\n"

  it "should throw if alias before anchor" $ do
    let vals = [toNode $ Alias "two", Node mempty (Just "two") "two"]
    runAsdfM (encodeNode $ toNode (Array vals)) `shouldSatisfy` P.throws @AsdfError P.anything


referenceSpec :: Spec
referenceSpec = do
  it "should encode a pointer" $ do
    let ref = JSONReference mempty (jsonPointer "#/users/1/name")
    (out, _) <- runAsdfM . encodeNode $ toNode $ Reference ref
    out `shouldBe` "{$ref: '#/users/1/name'}\n"

  it "should encode a reference" $ do
    let ref = JSONReference "https://woot.com/" (jsonPointer "#/users/1/name")
    (out, _) <- runAsdfM . encodeNode $ toNode $ Reference ref
    out `shouldBe` "{$ref: 'https://woot.com/#/users/1/name'}\n"

  it "should roundtrip reference" $ do
    let ref = Reference $ JSONReference mempty (jsonPointer "#/users/1/name")
    let tree = Object [("username", toNode ref)]
    out <- encodeM tree
    obj <- decodeM @Value out >>= expectObject
    lookup "username" obj `shouldBe` Just (Node mempty Nothing ref)


-- it "should rountrip and resolve references" $ do
--   let pn = PointyName "pip"
--   toValue pn `shouldBe` InternalRef (pointer "/names/2")
--
--   let pd = PointyData pn ["bob", "pip", "will"]
--   out <- encodeM pd
--   pd2 <- decodeM @PointyData out
--
--   pd2.other `shouldBe` PointyName "will"

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


streamSpec :: Spec
streamSpec = do
  it "should encode an empty string as empty single quotes" $ do
    let unit = fromValue $ String ""
    let obj = Object [("unit", unit)]
    (out, _) <- runAsdfM . encodeNode $ toNode obj
    out `shouldBe` "{unit: ''}\n"


documentSpec :: Spec
documentSpec = do
  it "converts to document" $ do
    asdf <- runAsdfM $ toAsdfDoc $ BasicData "henry"
    asdf.library.name `shouldBe` "telescope.hs"
    let Tree tree = asdf.tree
    lookup "username" tree `shouldBe` Just "henry"


blocksSpec :: Spec
blocksSpec = do
  it "includes blocks" $ do
    let ns = [1 .. 100]
    out <- encodeM (BasicArray ns)
    af <- runAsdfM $ splitAsdfFile out
    length af.blocks `shouldBe` 1

    [BlockData bd] <- runAsdfM $ mapM decodeBlock af.blocks
    bd `shouldBe` (toNDArray ns).bytes

  describe "index" $ do
    it "increeasing" $ do
      let nd1 = toNDArray ([1 .. 10] :: [Int64])
      let nd2 = toNDArray $ matrix @Int64 [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
      let blks = fmap (encodeBlock . BlockData) [nd1.bytes, nd2.bytes, nd1.bytes]
      let tree = "1234567890"
      let BlockIndex ix = blockIndex tree blks
      length ix `shouldBe` 3
      [i1, i2, i3] <- pure ix
      let start = BS.length tree.bytes
      i1 `shouldBe` start
      i2 `shouldSatisfy` P.gt (start + (10 * 8))
      i3 `shouldSatisfy` P.gt i2

    it "equivalent to example.asdf" $ do
      inp <- BS.readFile "samples/example.asdf"
      e <- decodeM @Value inp
      e `shouldSatisfy` P.con (Object P.anything)
      af <- runAsdfM $ toAsdfDoc e >>= encodeAsdf
      length af.blocks `shouldBe` 3

      let BlockIndex ix = blockIndex af.tree af.blocks
      length ix `shouldBe` 3
      (i1 : _) <- pure ix

      i1 `shouldBe` BS.length af.tree.bytes
      fmap (subtract i1) ix `shouldBe` fmap (subtract 897) [897, 1751, 2605]

    it "addresses blocks" $ do
      inp <- BS.readFile "samples/example.asdf"
      e <- decodeM @Value inp
      o <- encodeM e
      BlockIndex ix <- runAsdfM $ do
        a <- toAsdfDoc e
        af <- encodeAsdf a
        pure $ blockIndex af.tree af.blocks
      forM_ ix $ \n -> do
        BS.take 4 (BS.drop n o) `shouldBe` blockMagicToken


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


externalSpec :: Spec
externalSpec = do
  it "saves encoded document to an asdf for external verification in python" $ do
    let sd = SomeData 24 ["one", "two"] $ matrix [[1, 2, 3], [4, 5, 6]]
    out <- encodeM sd
    BS.writeFile "samples/generated.asdf" out


matrix :: (M.Prim n) => [[n]] -> Array D Ix2 n
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
    val -> expected "BasicArray.values" val


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

-- data PointyData = PointyData
--   { other :: PointyName
--   , names :: [Text]
--   }
--   deriving (Generic, FromAsdf, ToAsdf, Show, Eq)
--
--
-- newtype PointyName = PointyName Text
--   deriving (Show, Eq)
-- instance ToAsdf PointyName where
--   toValue (PointyName _) = InternalRef (pointer "/names/2")
-- instance FromAsdf PointyName where
--   parseValue = \case
--     String s -> pure $ PointyName s
--     InternalRef p -> parsePointer p
--     other -> expected "PointyName Ref" other
--
