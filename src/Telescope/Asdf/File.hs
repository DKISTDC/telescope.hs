{-# LANGUAGE RecordWildCards #-}

module Telescope.Asdf.File where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Word
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Telescope.Asdf.Error (AsdfError (..))


{- | The top-level document is always an object with keys: https://asdf-standard.readthedocs.io/en/latest/generated/stsci.edu/asdf/core/asdf-1.1.0.html#core-asdf-1-1-0
should we parse the software, etc?
yeah, we definitely should
-}

-- newtype Document = Document
--   { library :: Software
--   , history :: History
--   , tree :: Object
--   }

-- | Uncompressed, block data
newtype BlockData = BlockData {bytes :: ByteString}
  deriving (Eq)


newtype BlockSource = BlockSource Int
  deriving (Eq)


instance Show BlockData where
  show (BlockData bs) = "BlockData " <> show (BS.length bs)


-- what's the best way to represent a fixed-length ascii string
data BlockHeader = BlockHeader
  { headerSize :: Word16
  , -- , flags :: Word32 -- TODO support streamed
    compression :: Compression
  , allocatedSize :: Word64
  , usedSize :: Word64
  , dataSize :: Word64
  , checksum :: Checksum
  }
  deriving (Show, Eq)


-- a bunch of byte offsets
-- yeah, it's fast to lookup with (!?)
newtype BlockIndex = BlockIndex [Int]


data Compression
  = NoCompression -- "\0\0\0\0"
  | ZLib -- "zlib"
  | BZip2 -- "bzp2"
  deriving (Show, Eq)


newtype Checksum = Checksum ByteString
  deriving (Show, Eq)


noChecksum :: Checksum
noChecksum = Checksum $ BC.replicate 16 '0'


-- https://asdf-standard.readthedocs.io/en/latest/file_layout.html
-- #ASDF 1.0.0
-- #ASDF_STANDARD 1.5.0 (comments)
-- %YAML 1.1
-- %TAG ! tag:stsci.edu:asdf/
-- --- !core/asdf-1.1.0
-- asdf_library: !core/software-1.0.0 {author: The ASDF Developers, homepage: 'http://github.com/asdf-format/asdf', name: asdf, version: 2.11.0}

-- Header
-- Comments, optional
-- Tree, optional
-- Zero or more Blocks
-- Block Index, optional

-- TODO: parse the tree as yaml using libyaml
-- TEST: lots of things
data AsdfFile = AsdfFile
  { tree :: ByteString
  , blocks :: [BlockData]
  , index :: ByteString
  }
  deriving (Show, Eq)


-- INFO: Block Format
--   block_magic_token = ascii: "\323BLK"
--   header_size: UInt16 = size of remainder of the header in bytes
--   flags: UInt32
--   compression: Ascii[4] - "\0\0\0\0\" for no compression
--   allocated_size: UInt64 uint - size of the block data in bytes
--   used_size: UInt64 - amount of space used in the block
--   data_size: UInt64 - size of block when decoded. If compression = 0, == used_size
--   checksum: ascii[16] optional MD5 checksum. If "000000..." means no checksum should be performed
-- INFO: Flags: STREAMED = 0x1 - block extends to the end of the file. Ignore all the size fields. Must be last
-- INFO: Compression: zlib, bzp2
-- INFO: used_space bytes of meaningful data, then allocated_space - used_space bytes of unused data
--
-- INFO: Block Index
--  optional, if not present, you can "skip along" to find the location of blocks
--  should detect invalid or obselete indices and regenerate the index

-- TODO: To find the beginning of the first block, ASDF parsers should search from the end of the tree for the first occurrence of the block_magic_token
-- TODO: find the block index by reading backwards in the file
--  ensure the first offset entry matches the location of the first block in the file. If not, do not use!
--  ensure the last entry in the index refers to a block magic token, and the end of its allocated_space is followed by the block index
--  when using the index, make sure the block magic token exists at that index

-- test :: IO ()
-- test = do
--   -- TEST: the tree shouldn't contain any binary data!
--   putStrLn "TEST"
--   inp <- BS.readFile "/Users/shess/Data/VISP_L1_20230501T185359_AOPPO.asdf"
--   dp <- runEff $ runFailIO $ splitDocument inp
--   -- nope, it's the whole document
--   print $ BS.length inp
--   print $ BS.length dp.tree
--   print $ length dp.blocks
--   print $ BS.length dp.index
--
--   n <- runEff $ runFailIO $ parseTree dp.tree
--   print n

-- runEff $ testTree dp.tree

splitAsdfFile :: (Error AsdfError :> es) => ByteString -> Eff es AsdfFile
splitAsdfFile dat = evalState dat $ do
  tree <- parseToFirstBlock
  blocks <- parseBlocks
  index <- get
  pure $ AsdfFile{tree, blocks, index}
 where
  parseToFirstBlock = state $ BS.breakSubstring blockMagicToken

  parseBlocks :: (State ByteString :> es, Error AsdfError :> es) => Eff es [BlockData]
  parseBlocks = do
    inp <- get
    case runGetOrFail getBlocks (BL.fromStrict inp) of
      Left (_, num, err) -> throwError $ BlockError $ "at " ++ show num ++ ": " ++ err
      Right (rest, _, bs) -> do
        put (BL.toStrict rest)
        pure bs


-- where
-- handle = \case
--   EventDocumentStart -> _
--   _ -> _

getBlock :: Get BlockData
getBlock = do
  h <- getBlockHeader
  getBlockData h


-- | Skip along blocks and create a list of all of them
getBlocks :: Get [BlockData]
getBlocks = do
  isBlock <- checkMagicToken
  if not isBlock
    then pure []
    else do
      b <- getBlock
      bs <- getBlocks
      pure (b : bs)


getBlockHeader :: Get BlockHeader
getBlockHeader = do
  expectMagicToken
  headerSize <- label "header_size" getWord16be
  _flags <- label "flags" getWord32be
  compression <- label "compression" getCompression
  allocatedSize <- label "allocated_size" getWord64be
  usedSize <- label "used_size" getWord64be
  dataSize <- label "data_size" getWord64be
  checksum <- label "checksum" getChecksum
  pure $ BlockHeader{..}
 where
  getCompression = do
    val <- getByteString 4
    case val of
      "\0\0\0\0" -> pure NoCompression
      "zlib" -> pure ZLib
      "bzp2" -> pure BZip2
      _ -> fail $ "BlockHeader compression invalid, found " <> show val

  getChecksum = Checksum <$> getByteString 16

  expectMagicToken = do
    m <- getMagicToken
    case m of
      Right a -> pure a
      Left str -> fail $ "BlockHeader magic token invalid: " ++ show str


putBlockHeader :: BlockHeader -> Put
putBlockHeader h = do
  putByteString blockMagicToken
  putWord16be h.headerSize
  putWord32be 0 -- flags
  putCompression NoCompression
  putWord64be h.allocatedSize
  putWord64be h.usedSize
  putWord64be h.dataSize
  putChecksum h.checksum
 where
  putCompression _ = putByteString "\0\0\0\0"
  putChecksum (Checksum cs) = putByteString cs


putBlocks :: [BlockData] -> Put
putBlocks = mapM_ putBlock


putBlock :: BlockData -> Put
putBlock bd@(BlockData bs) = do
  putBlockHeader $ blockHeader bd
  putByteString bs


blockHeader :: BlockData -> BlockHeader
blockHeader (BlockData bs) =
  let bytes = fromIntegral $ BS.length bs
   in BlockHeader
        { headerSize = 0
        , compression = NoCompression
        , allocatedSize = bytes
        , usedSize = bytes
        , dataSize = bytes
        , checksum = noChecksum
        }


getBlockData :: BlockHeader -> Get BlockData
getBlockData h = do
  -- TODO: handle compression
  bytes <- getByteString $ fromIntegral h.usedSize
  _empty <- getByteString $ fromIntegral $ h.allocatedSize - h.usedSize
  pure $ BlockData bytes


blockMagicToken :: ByteString
blockMagicToken = BS.pack [0xd3, 0x42, 0x4c, 0x4b]


blockIndexHeader :: ByteString
blockIndexHeader = "#ASDF BLOCK INDEX"


-- TEST: make sure this doesn't consume any input
checkMagicToken :: Get Bool
checkMagicToken = do
  empty <- isEmpty
  if empty
    then pure False
    else do
      eb <- lookAhead getMagicToken
      pure $ either (const False) (const True) eb


-- consumes input
getMagicToken :: Get (Either ByteString ())
getMagicToken = do
  -- this still fails if it is empty
  str <- getByteString 4
  pure $
    if str == blockMagicToken
      then Right ()
      else Left str
