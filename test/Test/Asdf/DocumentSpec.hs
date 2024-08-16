module Test.Asdf.DocumentSpec where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Fail
import Skeletest
import Skeletest.Predicate ((>>>))
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Document


spec :: Spec
spec = do
  describe "blocks" testBlocks
  describe "document" testSplitDocument


testBlocks :: Spec
testBlocks = do
  let blockHeadZero = BlockHeader 1 NoCompression 0 0 0 noChecksum

  describe "BlockHeader" $ do
    it "should parse zero" $ do
      let bh = runPut (putBlockHeader blockHeadZero)
      goGet getBlockHeader bh `shouldBe` Right blockHeadZero

    it "should create from data" $ do
      let block = BlockData "hello"
      let bh = blockHeader block

      bh.allocatedSize `shouldBe` 5
      bh.usedSize `shouldBe` 5
      bh.allocatedSize `shouldBe` 5
      bh.usedSize `shouldBe` 5
      bh.dataSize `shouldBe` 5

  describe "putBlock" $ do
    it "puts a header" $ do
      runPut (putBlockHeader blockHeadZero) `shouldSatisfy` (BL.length >>> P.gt 10)

    it "puts a block" $ do
      runPut (putBlock $ BlockData "hello") `shouldSatisfy` (BL.length >>> P.gt 10)

  describe "getBlock" $ do
    it "fails on empty data" $ do
      goGet getBlock "" `shouldSatisfy` P.left P.anything

    it "fails if missing magic byte" $ do
      goGet getBlock "asdf" `shouldSatisfy` P.left P.anything

    it "puts an empty block" $ do
      let out = runPut (putBlock $ BlockData "")
      goGet getBlock out `shouldBe` Right (BlockData "")

  describe "getBlocks" $ do
    it "looks ahead without failing" $ do
      goGet checkMagicToken "asdf" `shouldBe` Right False

    it "gets a single block" $ do
      let out = runPut (putBlock $ BlockData "")
      goGet getBlocks out `shouldBe` Right [BlockData ""]

    it "gets empty array" $ do
      goGet getBlocks "" `shouldBe` Right []
 where
  goGet a bs =
    case runGetOrFail a bs of
      Left e -> Left e
      Right (_, _, x) -> Right x


testSplitDocument :: Spec
testSplitDocument = do
  describe "basic data" $ do
    it "should parse empty" $ do
      split "" `shouldBe` Right (DocumentParts "" [] "")

    it "optional blocks" $ do
      split "asdf" `shouldBe` Right (DocumentParts "asdf" [] "")

    it "optional tree" $ do
      let out = runPut $ putBlock $ BlockData "hello"
      split (BL.toStrict out) `shouldBe` Right (DocumentParts "" [BlockData "hello"] "")

    it "all parts" $ do
      let out = runPut $ putByteString "#hello\n" >> putBlock (BlockData "hello") >> putByteString "#index"
      split (BL.toStrict out) `shouldBe` Right (DocumentParts "#hello\n" [BlockData "hello"] "#index")

  describe "real asdf file" $ do
    it "tree smaller than document " $ do
      SampleFixture inp dp <- getFixture
      BS.length dp.tree `shouldNotBe` BS.length inp

    it "tree exists" $ do
      SampleFixture _ dp <- getFixture
      BS.length dp.tree `shouldSatisfy` P.gt 0

    it "blocks exist" $ do
      SampleFixture _ dp <- getFixture
      length dp.blocks `shouldSatisfy` P.gt 0

    it "index exists" $ do
      SampleFixture _ dp <- getFixture
      BS.length dp.index `shouldSatisfy` P.gt 0
 where
  split = runPureEff . runFail . splitDocument


data SampleFixture = SampleFixture {input :: BS.ByteString, parts :: DocumentParts}


instance Fixture SampleFixture where
  fixtureAction = do
    inp <- BS.readFile "samples/example.asdf"
    edp <- runEff $ runFail $ splitDocument inp
    case edp of
      Left e -> fail $ "Could not splitDocument:" ++ e
      Right dp -> pure $ noCleanup $ SampleFixture inp dp
