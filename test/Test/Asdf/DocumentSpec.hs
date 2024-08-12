module Test.Asdf.DocumentSpec where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Telescope.Asdf.Document
import Test.Syd


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
      bh.dataSize `shouldBe` 5

    it "should put and get" $ do
      let block = BlockData "hello"
      Right bh <- pure $ goGet getBlockHeader $ runPut (putBlockHeader $ blockHeader block)
      bh.allocatedSize `shouldBe` 5
      bh.usedSize `shouldBe` 5
      bh.dataSize `shouldBe` 5

  describe "putBlock" $ do
    it "puts a header" $ do
      runPut (putBlockHeader blockHeadZero) `shouldSatisfy` \bs -> BL.length bs > 10

    it "puts a block" $ do
      runPut (putBlock $ BlockData "hello") `shouldSatisfy` \bs -> BL.length bs > 10

  describe "getBlock" $ do
    it "fails on empty data" $ do
      goGet getBlock "" `shouldSatisfy` isLeft

    it "fails if missing magic byte" $ do
      goGet getBlock "asdf" `shouldSatisfy` isLeft

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
  it "should parse empty" $ do
    splitDocument "" `shouldBe` Right (DocumentParts "" [] "")

  it "optional blocks" $ do
    splitDocument "asdf" `shouldBe` Right (DocumentParts "asdf" [] "")

  it "optional tree" $ do
    let out = runPut $ putBlock $ BlockData "hello"
    splitDocument (BL.toStrict out) `shouldBe` Right (DocumentParts "" [BlockData "hello"] "")

  it "all parts" $ do
    let out = runPut $ putByteString "#hello\n" >> putBlock (BlockData "hello") >> putByteString "#index"
    splitDocument (BL.toStrict out) `shouldBe` Right (DocumentParts "#hello\n" [BlockData "hello"] "#index")
