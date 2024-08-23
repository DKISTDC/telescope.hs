{-# LANGUAGE RecordWildCards #-}

module Telescope.Asdf.File where

import Control.Monad (forM)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
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
  , -- , flags :: Word32 -- TODO support streamed
    compression :: Compression
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
  { tree :: ByteString
  , blocks :: [BlockData]
  , index :: ByteString
  }
  deriving (Show, Eq)


splitAsdfFile :: (Error AsdfError :> es) => ByteString -> Eff es AsdfFile
splitAsdfFile dat = evalState dat $ do
  tree <- parseToFirstBlock
  blocks <- parseBlocks
  index <- remainingBytes
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

  remainingBytes = get


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


encodeBlock :: BlockData -> EncodedBlock
encodeBlock b =
  EncodedBlock $ BL.toStrict $ runPut $ putBlock b


blockIndex :: ByteString -> [EncodedBlock] -> BlockIndex
blockIndex tree ebs =
  let ns = scanl go (BS.length tree) ebs
   in BlockIndex $ take (length ns - 1) ns
 where
  go :: Int -> EncodedBlock -> Int
  go n eb = n + BS.length eb.bytes


newtype EncodedBlock = EncodedBlock
  { bytes :: ByteString
  }


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
