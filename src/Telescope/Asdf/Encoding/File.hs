{-# LANGUAGE RecordWildCards #-}

module Telescope.Asdf.Encoding.File where

import Control.Monad (replicateM_)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.String (IsString)
import Data.Word
import Effectful
import Effectful.Error.Static
import Effectful.Fail
import Effectful.NonDet
import Effectful.State.Static.Local
import Telescope.Asdf.Error


splitAsdfFile :: (Error AsdfError :> es) => ByteString -> Eff es AsdfFile
splitAsdfFile dat = do
  res <- runFail $ evalState dat parseAsdfFile
  case res of
    Left e -> throwError $ ParseError e
    Right a -> pure a


parseAsdfFile :: (State ByteString :> es, Fail :> es) => Eff es AsdfFile
parseAsdfFile = do
  -- if it's empty, then give an error
  tree <- onEmpty "tree" parseTree
  blocks <- onEmpty "blocks" parseBlocks
  index <- parseIndex
  pure $ AsdfFile{tree, blocks, index}
 where
  onEmpty ex eff = do
    res <- runNonDet OnEmptyKeep eff
    case res of
      Left _ -> do
        inp <- get @ByteString
        fail $ "Expected " ++ ex ++ " at " ++ show (BS.take 100 inp)
      Right a -> pure a


parseTree :: (State ByteString :> es, NonDet :> es) => Eff es (Encoded Tree)
parseTree = do
  t <- parseUntil blockMagicToken <|> parseUntil blockIndexHeader <|> remainingBytes
  pure $ Encoded t


parseIndex :: (State ByteString :> es) => Eff es (Encoded Index)
parseIndex =
  Encoded <$> get


parseBlocks :: (State ByteString :> es, NonDet :> es) => Eff es [Encoded Block]
parseBlocks = many parseBlock


parseBlock :: (State ByteString :> es, NonDet :> es) => Eff es (Encoded Block)
parseBlock = do
  exactly blockMagicToken
  b <- parseUntil blockMagicToken <|> parseUntil blockIndexHeader <|> nonEmpty
  case b of
    "" -> empty
    _ -> pure $ Encoded $ blockMagicToken <> b


exactly :: (State ByteString :> es, NonDet :> es) => ByteString -> Eff es ()
exactly val = do
  inp <- get @ByteString
  if val `BS.isPrefixOf` inp
    then do
      put $ BS.drop (BS.length val) inp
      pure ()
    else empty


parseUntil :: (State ByteString :> es, NonDet :> es) => ByteString -> Eff es ByteString
parseUntil val = do
  inp <- get @ByteString
  let (before, rest) = BS.breakSubstring val inp
  case rest of
    "" -> empty
    _ -> do
      put rest
      pure before


nonEmpty :: (NonDet :> es, State ByteString :> es) => Eff es ByteString
nonEmpty = do
  b <- remainingBytes
  case b of
    "" -> empty
    _ -> pure b


remainingBytes :: (NonDet :> es, State ByteString :> es) => Eff es ByteString
remainingBytes = do
  inp <- get
  put @ByteString ""
  pure inp


breakIndex :: (State ByteString :> es) => Eff es (Encoded Index)
breakIndex = Encoded <$> get


concatAsdfFile :: AsdfFile -> ByteString
concatAsdfFile a =
  mconcat [a.tree.bytes, blocks a.blocks, index a.index]
 where
  blocks :: [Encoded Block] -> ByteString
  blocks ebks = mconcat $ fmap (.bytes) ebks
  -- case mconcat $ fmap (.bytes) ebks of
  --   "" -> ""
  --   s -> s <> "\n"
  index ix = ix.bytes


encodeTree :: ByteString -> Encoded Tree
encodeTree tr =
  Encoded $ BS.intercalate "\n" (headers <> formatDoc tr) <> "\n" -- has a trailing newline
 where
  formatDoc doc = ["--- " <> doc, "..."]
  headers = ["#ASDF 1.0.0", "#ASDF_STANDARD 1.5.0", "%YAML 1.1", tagDirective]
  tagDirective = "%TAG ! tag:stsci.edu:asdf/"


encodeBlocks :: [BlockData] -> [Encoded Block]
encodeBlocks = fmap encodeBlock


encodeIndex :: BlockIndex -> Encoded Index
encodeIndex (BlockIndex ns) =
  Encoded $ BS.intercalate "\n" $ ["#ASDF BLOCK INDEX", "%YAML 1.1", "---"] <> fmap indexEntry ns <> ["..."]
 where
  indexEntry n = "- " <> BC.pack (show n)


data Index
data Tree
data Block


newtype Encoded a = Encoded {bytes :: ByteString}
  deriving (Show, Eq)
  deriving newtype (IsString)


-- | Decompressed block data
newtype BlockData = BlockData {bytes :: ByteString}
  deriving (Eq)


newtype BlockSource = BlockSource Int
  deriving (Eq)


instance Show BlockData where
  show (BlockData bs) = "BlockData " <> show (BS.length bs)


-- what's the best way to represent a fixed-length ascii string
data BlockHeader = BlockHeader
  { headerSize :: Word16
  , flags :: Word32
  , compression :: Compression
  , allocatedSize :: Word64
  , usedSize :: Word64
  , dataSize :: Word64
  , checksum :: Checksum
  }
  deriving (Show, Eq)


newtype BlockIndex = BlockIndex [Int]
  deriving (Eq)


data Compression
  = NoCompression -- "\0\0\0\0"
  | ZLib -- "zlib"
  | BZip2 -- "bzp2"
  deriving (Show, Eq)


newtype Checksum = Checksum ByteString
  deriving (Show, Eq)


noChecksum :: Checksum
noChecksum = Checksum $ BC.replicate 16 '0'


data AsdfFile = AsdfFile
  { tree :: Encoded Tree
  , blocks :: [Encoded Block]
  , index :: Encoded Index
  }
  deriving (Show, Eq)


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
  start <- bytesRead

  -- the remainder is inside the headerSize
  flags <- label "flags" getWord32be -- 4
  compression <- label "compression" getCompression -- 4
  allocatedSize <- label "allocated_size" getWord64be -- 8
  usedSize <- label "used_size" getWord64be -- 8
  dataSize <- label "data_size" getWord64be -- 8
  checksum <- label "checksum" getChecksum -- 2
  end <- bytesRead

  -- skip until the end of headerSize
  let usedHead = fromIntegral $ end - start
  skip $ fromIntegral headerSize - usedHead

  pure $ BlockHeader{..}
 where
  getCompression = do
    val <- getByteString 4
    case val of
      "\0\0\0\0" -> pure NoCompression
      -- "zlib" -> pure ZLib
      -- "bzp2" -> pure BZip2
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
  size <- putHeaderContent
  let emptyBytes = fromIntegral h.headerSize - size :: Int
  replicateM_ emptyBytes $ putWord8 0x0
 where
  putCompression _ = putByteString "\0\0\0\0"
  putChecksum (Checksum cs) = putByteString cs
  putHeaderContent = do
    let bs = runPut $ do
          putWord32be 0 -- flags
          putCompression NoCompression
          putWord64be h.allocatedSize
          putWord64be h.usedSize
          putWord64be h.dataSize
          putChecksum h.checksum
    putLazyByteString bs
    pure $ fromIntegral $ BL.length bs


encodeBlock :: BlockData -> Encoded Block
encodeBlock b =
  Encoded $ BL.toStrict $ runPut $ putBlock b


blockIndex :: Encoded Tree -> [Encoded Block] -> BlockIndex
blockIndex (Encoded bytes) ebs =
  let ns = scanl go (BS.length bytes) ebs
   in BlockIndex $ take (length ns - 1) ns
 where
  go :: Int -> Encoded Block -> Int
  go n eb = n + BS.length eb.bytes


putBlock :: BlockData -> Put
putBlock bd@(BlockData bs) = do
  putBlockHeader $ blockHeader bd
  putByteString bs


blockHeader :: BlockData -> BlockHeader
blockHeader (BlockData bs) =
  let bytes = fromIntegral $ BS.length bs
   in BlockHeader
        { headerSize = 48 -- minimum allowed size. Our encoding uses fewer bytes than this
        , compression = NoCompression
        , allocatedSize = bytes
        , usedSize = bytes
        , dataSize = bytes
        , checksum = noChecksum
        , flags = 0
        }


getBlockData :: BlockHeader -> Get BlockData
getBlockData h = do
  -- LATER: handle compression
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
  emp <- isEmpty
  if emp
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
