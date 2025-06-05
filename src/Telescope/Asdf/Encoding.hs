module Telescope.Asdf.Encoding where

import Conduit
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Dynamic
import Effectful.Resource
import Effectful.State.Static.Local
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Encoding.Stream
import Telescope.Asdf.Error
import Telescope.Asdf.Node
import Telescope.Data.Parser (ParseError, runParser)
import Text.Libyaml qualified as Yaml


-- | Encode a 'ToAsdf' to a 'ByteString'
encodeM :: (ToAsdf a, MonadIO m, MonadThrow m) => a -> m ByteString
encodeM a = runAsdfM $ encode a


-- | Encode a 'ToAsdf' to a 'ByteString'
encode :: (ToAsdf a, IOE :> es, Error AsdfError :> es) => a -> Eff es ByteString
encode a = do
  asdf <- toAsdfDoc a
  file <- encodeAsdf asdf
  pure $ concatAsdfFile file


-- | Encode an 'Asdf' document to a 'ByteString'
encodeAsdf :: (IOE :> es, Error AsdfError :> es) => Asdf -> Eff es AsdfFile
encodeAsdf a = do
  (doc, bds) <- encodeNode (toNode a)
  let tree = encodeTree doc
  let blocks = fmap encodeBlock bds
  let index = encodeIndex $ blockIndex tree blocks
  pure $ AsdfFile{tree, blocks, index}


-- | Low-level encoding of a node to a Yaml tree, without required headers, etc
encodeNode :: (IOE :> es, Error AsdfError :> es) => Node -> Eff es (ByteString, [BlockData])
encodeNode node = do
  runYamlError $ encodeStream (yieldDocumentStream $ yieldNode node)


-- | Create a stream of yaml events
encodeStream :: (IOE :> es, Error AsdfError :> es) => ConduitT () Yaml.Event (Eff (State Anchors : State [BlockData] : Resource : es)) () -> Eff es (ByteString, [BlockData])
encodeStream con = do
  runStream $ con .| Yaml.encodeWith format
 where
  format =
    Yaml.defaultFormatOptions
      & Yaml.setTagRendering Yaml.renderUriTags
      & Yaml.setWidth (Just 100)


-- | Decode a 'ByteString' to a 'FromAsdf'
decodeM :: (FromAsdf a, MonadIO m, MonadThrow m) => ByteString -> m a
decodeM bs = runAsdfM $ decode bs


-- | Decode a 'ByteString' to a 'FromAsdf'
decodeEither :: forall a m. (FromAsdf a, MonadIO m) => ByteString -> m (Either String a)
decodeEither bs = do
  res <- liftIO $ runEff $ runErrorNoCallStack @AsdfError $ decode @a bs
  pure $ either (Left . show) Right res


-- | Decode a 'ByteString' to a 'FromAsdf'
decode :: (FromAsdf a, IOE :> es, Error AsdfError :> es) => ByteString -> Eff es a
decode bs = do
  f <- splitAsdfFile bs
  tree <- parseAsdfTree f.tree f.blocks
  decodeFromTree tree


-- | Decode a 'Tree' to a 'FromAsdf'
decodeFromTree :: forall a es. (Error AsdfError :> es) => (FromAsdf a) => Tree -> Eff es a
decodeFromTree (Tree o) = do
  runParseError $ runParser $ parseValue @a (Object o)


-- | Parse the asdf file parts into a 'Tree'
parseAsdfTree :: (Error AsdfError :> es, IOE :> es) => Encoded Tree -> [Encoded Block] -> Eff es Tree
parseAsdfTree etree eblks = do
  (root, _) <- streamAsdfFile etree eblks
  pure $ Tree root


streamAsdfFile :: (Error AsdfError :> es, IOE :> es) => Encoded Tree -> [Encoded Block] -> Eff es (Object, Anchors)
streamAsdfFile (Encoded inp) ebks = do
  blocks <- mapM decodeBlock ebks
  runYamlError $ do
    runReader blocks . runState @Anchors mempty . runResource . runConduit $ Yaml.decode inp .| sinkTree


decodeBlock :: (Error AsdfError :> es) => Encoded Block -> Eff es BlockData
decodeBlock (Encoded blk) = do
  case runGetOrFail getBlock (BL.fromStrict blk) of
    Left (_, num, err) -> throwError $ BlockError $ "at " ++ show num ++ ": " ++ err
    Right ("", _, b) -> pure b
    Right (rest, _, _) -> throwError $ BlockError $ "Unused bytes: " ++ show rest


-- | Decode the BlockIndex
decodeBlockIndex :: (Error AsdfError :> es, IOE :> es) => ByteString -> Eff es BlockIndex
decodeBlockIndex inp =
  runYamlError . runResource . runConduit $ Yaml.decode inp .| sinkIndex


runYamlError :: (Error AsdfError :> es) => Eff (Error YamlError : es) a -> Eff es a
runYamlError = runErrorNoCallStackWith @YamlError (throwError . YamlError . show)


runParseError :: (Error AsdfError :> es) => Eff (Error ParseError : es) a -> Eff es a
runParseError = runErrorNoCallStackWith @ParseError (throwError . ParseError . show)
