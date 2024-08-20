module Test.Asdf.FileSpec where

import Control.Monad.Catch (throwM)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Error.Static
import Skeletest
import Skeletest.Predicate ((>>>))
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Error
import Telescope.Asdf.File


spec :: Spec
spec = do
  describe "blocks" testBlocks
  describe "split-file" testSplit


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


testSplit :: Spec
testSplit = do
  describe "basic data" $ do
    it "should parse empty" $ do
      split "" `shouldBe` Right (AsdfFile "" [] "")

    it "optional blocks" $ do
      split "asdf" `shouldBe` Right (AsdfFile "asdf" [] "")

    it "optional tree" $ do
      let out = runPut $ putBlock $ BlockData "hello"
      split (BL.toStrict out) `shouldBe` Right (AsdfFile "" [BlockData "hello"] "")

    it "all parts" $ do
      let out = runPut $ putByteString "#hello\n" >> putBlock (BlockData "hello") >> putByteString "#index"
      split (BL.toStrict out) `shouldBe` Right (AsdfFile "#hello\n" [BlockData "hello"] "#index")

  describe "real asdf file" $ do
    it "tree smaller than document " $ do
      ExampleFileFix inp dp <- getFixture
      BS.length dp.tree `shouldNotBe` BS.length inp

    it "tree exists" $ do
      ExampleFileFix _ dp <- getFixture
      BS.length dp.tree `shouldSatisfy` P.gt 0

    it "blocks exist" $ do
      ExampleFileFix _ dp <- getFixture
      length dp.blocks `shouldSatisfy` P.gt 0

    it "index exists" $ do
      ExampleFileFix _ dp <- getFixture
      BS.length dp.index `shouldSatisfy` P.gt 0
 where
  split = runPureEff . runErrorNoCallStack @AsdfError . splitAsdfFile


data ExampleFileFix = ExampleFileFix {input :: BS.ByteString, file :: AsdfFile}
instance Fixture ExampleFileFix where
  fixtureAction = do
    inp <- BS.readFile "samples/example.asdf"
    f <- runEff $ runErrorNoCallStackWith @AsdfError throwM $ splitAsdfFile inp
    pure $ noCleanup $ ExampleFileFix inp f
