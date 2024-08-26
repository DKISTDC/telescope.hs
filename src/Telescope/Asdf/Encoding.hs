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
import Telescope.Asdf.Parser (ParseError, fromParser)
import Text.Libyaml qualified as Yaml


encodeM :: (ToAsdf a, MonadIO m, MonadThrow m) => a -> m ByteString
encodeM a = runAsdfM $ encode a


encode :: (ToAsdf a, IOE :> es, Error AsdfError :> es) => a -> Eff es ByteString
encode a = do
  asdf <- toAsdfDoc a
  file <- encodeToAsdfFile asdf
  pure $ concatAsdfFile file


-- | Encode the tree and the data blocks
encodeToAsdfFile :: (IOE :> es, Error AsdfError :> es) => Asdf -> Eff es AsdfFile
encodeToAsdfFile a = do
  (doc, bds) <- runResource . runState @[BlockData] [] . runConduit $ yieldDocument a .| Yaml.encodeWith format
  let tree = encodeTree doc
  let blocks = fmap encodeBlock bds
  let index = encodeIndex $ blockIndex tree blocks
  pure $
    AsdfFile{tree, blocks, index}
 where
  format = Yaml.defaultFormatOptions & Yaml.setTagRendering Yaml.renderUriTags


decodeM :: (FromAsdf a, MonadIO m, MonadThrow m) => ByteString -> m a
decodeM bs = runAsdfM $ decode bs


decodeEither :: forall a m. (FromAsdf a, MonadIO m) => ByteString -> m (Either String a)
decodeEither bs = do
  res <- liftIO $ runEff $ runErrorNoCallStack @AsdfError $ decode @a bs
  pure $ either (Left . show) Right res


decode :: (FromAsdf a, IOE :> es, Error AsdfError :> es) => ByteString -> Eff es a
decode bs = do
  f <- splitAsdfFile bs
  asdf <- fromAsdfFile f.tree f.blocks
  runParseError $ fromParser $ parseValue $ Object asdf.tree


fromAsdfFile :: (Error AsdfError :> es, IOE :> es) => Encoded Tree -> [Encoded Block] -> Eff es Asdf
fromAsdfFile (Encoded inp) ebks = do
  blocks <- mapM decodeBlock ebks
  runParseError . runYamlError . runResource . runReader blocks . runConduit $ Yaml.decode inp .| sinkAsdf


decodeBlock :: (Error AsdfError :> es) => Encoded Block -> Eff es BlockData
decodeBlock (Encoded blk) = do
  case runGetOrFail getBlock (BL.fromStrict blk) of
    Left (_, num, err) -> throwError $ BlockError $ "at " ++ show num ++ ": " ++ err
    Right ("", _, b) -> pure b
    Right (rest, _, _) -> throwError $ BlockError $ "Unused bytes: " ++ show rest


decodeBlockIndex :: (Error AsdfError :> es, IOE :> es) => ByteString -> Eff es BlockIndex
decodeBlockIndex inp =
  runYamlError . runResource . runConduit $ Yaml.decode inp .| sinkIndex


runYamlError :: (Error AsdfError :> es) => Eff (Error YamlError : es) a -> Eff es a
runYamlError = runErrorNoCallStackWith @YamlError (throwError . YamlError . show)


runParseError :: (Error AsdfError :> es) => Eff (Error ParseError : es) a -> Eff es a
runParseError = runErrorNoCallStackWith @ParseError (throwError . ParseError . show)
