module Test.Asdf.FileSpec where

import Control.Monad.Catch (throwM)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Effectful
import Effectful.Error.Static
import Effectful.Fail
import Effectful.NonDet
import Effectful.State.Static.Local
import Skeletest
import Skeletest.Predicate ((>>>))
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Error


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
  describe "parse tree" $ withMarkers ["focus"] $ do
    it "empty" $ do
      t <- runParse "" parseTree
      t `shouldBe` Encoded ""

    it "tree only" $ do
      (t, rest) <- runParse "value" $ do
        t <- parseTree
        rest <- get @ByteString
        pure (t, rest)
      t `shouldBe` Encoded "value"
      rest `shouldBe` ""

    it "tree with data" $ do
      let Encoded bks = encodeBlock (BlockData "woot")
      let inp = "value\n" <> bks
      t <- runParse inp parseTree
      t `shouldBe` Encoded "value\n"

    it "tree with index" $ do
      let Encoded ix = encodeIndex (BlockIndex [4])
      let inp = "value\n" <> ix
      (t, rest) <- runParse inp $ do
        t <- parseTree
        rest <- get @ByteString
        pure (t, rest)
      t `shouldBe` Encoded "value\n"
      rest `shouldBe` ix

    it "tree with all" $ do
      let Encoded bks = encodeBlock (BlockData "woot")
      let Encoded ix = encodeIndex (BlockIndex [4])
      let inp = "value\n" <> bks <> ix
      t <- runParse inp parseTree
      t `shouldBe` Encoded "value\n"

  describe "parse blocks" $ withMarkers ["focus"] $ do
    it "one block" $ do
      let Encoded blks = encodeBlock (BlockData "woot")
      n <- runParse blks parseBlock
      n `shouldBe` Encoded blks

    it "first block" $ do
      let Encoded b1 = encodeBlock (BlockData "b1")
      let Encoded b2 = encodeBlock (BlockData "b2")
      b <- runParse (b1 <> b2) parseBlock
      b `shouldBe` Encoded b1

    it "block with index" $ do
      let Encoded b1 = encodeBlock (BlockData "b1")
      let Encoded ix = encodeIndex (BlockIndex [10])
      let inp = b1 <> ix
      n <- runParse inp parseBlock
      n `shouldBe` Encoded b1

    it "blocks" $ do
      let Encoded b1 = encodeBlock (BlockData "b1")
      let Encoded b2 = encodeBlock (BlockData "b2")
      let inp = b1 <> b2
      bs <- runParse inp parseBlocks
      bs `shouldBe` [Encoded b1, Encoded b2]

    it "blocks and index" $ do
      let Encoded b1 = encodeBlock (BlockData "b1")
      let Encoded b2 = encodeBlock (BlockData "b2")
      let Encoded ix = encodeIndex (BlockIndex [10])
      let inp = b1 <> b2 <> ix
      bs <- runParse inp parseBlocks
      bs `shouldBe` [Encoded b1, Encoded b2]

  describe "basic data" $ do
    it "should parse empty" $ do
      af <- split ""
      af `shouldBe` AsdfFile "" [] ""

    it "just a tree" $ do
      af <- split "tree"
      af.tree `shouldBe` "tree"
      af.blocks `shouldBe` []
      af.index `shouldBe` ""

  describe "optionals" $ do
    it "tree + data!" $ do
      let treeData = "#hello"
      let tree = Encoded treeData
      let blocks = [encodeBlock (BlockData "data")]
      let ix = encodeIndex (blockIndex tree blocks)
      let inp = concatAsdfFile $ AsdfFile{tree = Encoded treeData, blocks, index = ix}
      af <- split inp
      af.tree `shouldBe` "#hello"
      af.blocks `shouldBe` blocks
      af.index `shouldBe` ix

    it "all parts" $ do
      let treeData = "#hello\n"
      let blocks = [encodeBlock (BlockData "data")]
      let index = encodeIndex (BlockIndex [BS.length treeData])
      let out = concatAsdfFile $ AsdfFile{tree = Encoded treeData, blocks, index}
      af <- split out
      af.tree `shouldBe` Encoded "#hello\n"
      af.blocks `shouldBe` blocks
      af.index `shouldBe` index

    it "optional blocks" $ do
      af <- split "asdf"
      af `shouldBe` AsdfFile "asdf" [] ""

    it "optional tree" $ do
      let Encoded out = encodeBlock $ BlockData "hello"
      af <- split out
      af.tree `shouldBe` Encoded ""
      af.blocks `shouldBe` [Encoded out]
      af.index `shouldBe` ""

    it "tree and index" $ do
      let out = "tree\n" <> blockIndexHeader <> "index"
      af <- split out
      af.tree `shouldBe` "tree\n"
      af.blocks `shouldBe` []
      af.index `shouldBe` Encoded (blockIndexHeader <> "index")

  describe "real asdf file" $ do
    it "tree smaller than document " $ do
      ExampleFileFix inp dp <- getFixture
      BS.length dp.tree.bytes `shouldNotBe` BS.length inp

    it "tree exists" $ do
      ExampleFileFix _ dp <- getFixture
      BS.length dp.tree.bytes `shouldSatisfy` P.gt 0

    it "blocks exist" $ do
      ExampleFileFix _ dp <- getFixture
      length dp.blocks `shouldSatisfy` P.gt 0

    it "index exists" $ do
      ExampleFileFix _ dp <- getFixture
      BS.length dp.index.bytes `shouldSatisfy` P.gt 0
 where
  split :: BS.ByteString -> IO AsdfFile
  split inp = runAsdfM $ splitAsdfFile inp

  runParse :: ByteString -> Eff [NonDet, State ByteString, IOE] a -> IO a
  runParse inp eff = do
    ret <- runEff . runState inp . runNonDet OnEmptyKeep $ eff
    case ret of
      (Left _, rest) -> fail $ "Failed parse at: " ++ show (BS.take 100 rest)
      (Right a, _) -> pure a


data ExampleFileFix = ExampleFileFix {input :: BS.ByteString, file :: AsdfFile}
instance Fixture ExampleFileFix where
  fixtureAction = do
    inp <- BS.readFile "samples/example.asdf"
    f <- runAsdfM $ splitAsdfFile inp
    pure $ noCleanup $ ExampleFileFix inp f
