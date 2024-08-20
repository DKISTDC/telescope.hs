module Telescope.Asdf.Decoding where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators (peek)
import Data.Conduit.Combinators qualified as C
import Data.List ((!?))
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Error.Static
import Effectful.NonDet
import Effectful.Reader.Dynamic
import Effectful.Resource
import System.ByteOrder
import Telescope.Asdf.Class (FromAsdf (..))
import Telescope.Asdf.Core (Asdf (..))
import Telescope.Asdf.Error (AsdfError (..))
import Telescope.Asdf.File (AsdfFile (..), BlockData (..), splitAsdfFile)
import Telescope.Asdf.Node
import Telescope.Asdf.Parser (ParseError, fromParser)
import Telescope.Data.Axes
import Text.Libyaml (Event (..), Tag (..))
import Text.Libyaml qualified as Yaml
import Text.Read (readMaybe)


-- you can't catch monadfail, can you?
-- that's not ideal
-- throwing an error might be better...
-- decode :: forall a m. (FromAsdf a, MonadIO m, MonadThrow m) => ByteString -> m a
-- decode bs = liftIO $ runEff $ runErrorNoCallStackWith throwM $ do
--   f <- splitFile bs
--   asdf <- parseAsdf f.tree f.blocks
--   case runParser $ parseValue $ Object asdf.tree of
--     Left e -> fail $ "Parse Error: " ++ e
--     Right a -> pure a

decode :: (FromAsdf a, IOE :> es, Error AsdfError :> es) => ByteString -> Eff es a
decode bs = do
  f <- splitAsdfFile bs
  asdf <- fromAsdfFile f.tree f.blocks
  runParseError $ fromParser $ parseValue $ Object asdf.tree
 where
  runParseError = runErrorNoCallStackWith @ParseError (throwError . ParseError . show)


-- WARNING: IOE is required to get the MonadResource instance! Dumb
-- wild, they use unsafePerformIO
-- https://hackage.haskell.org/package/yaml-0.11.11.2/docs/src/Data.Yaml.html#decodeEither%27

fromAsdfFile :: (Error AsdfError :> es, IOE :> es) => ByteString -> [BlockData] -> Eff es Asdf
fromAsdfFile inp blocks = do
  runParseError . runYamlError . runResource . runReader blocks . runConduit $ Yaml.decode inp .| sinkAsdf
 where
  runYamlError = runErrorNoCallStackWith @YamlError (throwError . YamlError . show)
  runParseError = runErrorNoCallStackWith @ParseError (throwError . ParseError . show)


sinkAsdf :: (Error YamlError :> es, Error ParseError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Asdf
sinkAsdf = do
  tree <- sinkTree
  lift $ fromParser (parseValue $ Object tree)


sinkTree :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Object
sinkTree = do
  expect EventStreamStart
  expect EventDocumentStart
  Node _ v <- sinkNode
  case v of
    Object o -> pure o
    _ -> lift $ throwError $ InvalidTree "Expected Object" v


-- NOTE: START HERE
-- TODO: flesh these out
-- TODO: sinkDocument, should consume everything and produce a single document
sinkNode :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Node
sinkNode = do
  e <- event
  case e of
    EventScalar s t _ _ -> lift $ parseScalar s t
    EventMappingStart tg _ _ -> do
      let stag = tag tg
      if isNDArray stag
        then sinkNDArray stag
        else sinkObject stag
    EventSequenceStart tg _ _ -> do
      ns <- sinkSequence
      pure $ Node (tag tg) $ Array ns
    ev -> lift $ throwError $ ExpectedEvent "Not Handled" ev
 where
  sinkObject stag = do
    kvs <- sinkMappings
    pure $ Node stag $ Object kvs

  sinkNDArray stag = do
    dat <- sinkNDArrayData
    pure $ Node stag $ NDArray dat


sinkNDArrayData :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) NDArrayData
sinkNDArrayData = do
  kvs <- sinkMappings
  bytes <- require "source" kvs >>= findSource
  datatype <- require "datatype" kvs >>= parseDatatype
  byteorder <- require "byteorder" kvs >>= parseByteorder
  shape <- require "shape" kvs >>= parseShape
  pure $ NDArrayData{bytes, datatype, byteorder, shape}
 where
  require key kvs =
    case lookup key kvs of
      Nothing -> lift $ throwError $ NDArrayMissingKey (unpack key)
      Just (Node _ val) -> pure val

  parseDatatype val =
    case val of
      String "int64" -> pure Int64
      String "float64" -> pure Float64
      _ -> lift $ throwError $ NDArrayExpected "DataType" val

  parseByteorder val =
    case val of
      String "little" -> pure LittleEndian
      String "big" -> pure BigEndian
      _ -> lift $ throwError $ NDArrayExpected "Byteorder" val

  parseShape val =
    case val of
      Array ns -> axesRowMajor <$> mapM (parseAxis . (.value)) ns
      _ -> lift $ throwError $ NDArrayExpected "Shape" val

  parseAxis val =
    case val of
      Integer n -> pure $ fromIntegral n
      _ -> lift $ throwError $ NDArrayExpected "Shape Axis" val

  findSource val =
    case val of
      Integer s -> do
        blocks <- lift ask
        case blocks !? fromIntegral s of
          Nothing -> lift $ throwError $ NDArrayMissingBlock s
          Just (BlockData b) -> pure b
      _ -> lift $ throwError $ NDArrayExpected "Source" val


sinkMapping :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) (Key, Node)
sinkMapping = do
  k <- sinkMapKey
  v <- sinkNode
  pure (k, v)
 where
  sinkMapKey =
    event >>= \case
      EventScalar s _ _ _ -> pure $ decodeUtf8 s
      ev -> lift $ throwError $ ExpectedEvent "Scalar Key" ev


sinkMappings :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [(Key, Node)]
sinkMappings = do
  sinkWhile (/= EventMappingEnd) sinkMapping


-- oh, the event mapping ends aren't being consumed!
sinkWhile :: (Event -> Bool) -> ConduitT Event o (Eff es) a -> ConduitT Event o (Eff es) [a]
sinkWhile p parse = do
  e <- peek
  if maybe False p e
    then do
      a <- parse
      as <- sinkWhile p parse
      pure (a : reverse as)
    else do
      -- consume the one we matched
      C.drop 1
      pure []


sinkSequence :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [Node]
sinkSequence = do
  sinkWhile (/= EventSequenceEnd) sinkNode


parseScalar :: (Error YamlError :> es) => ByteString -> Yaml.Tag -> Eff es Node
parseScalar inp tg = byTag tg
 where
  byTag :: (Error YamlError :> es) => Yaml.Tag -> Eff es Node
  byTag = \case
    StrTag -> fromValue <$> parseStr inp -- always succeeds
    FloatTag -> throwEmpty "Float" $ fromValue <$> parseFloat inp
    IntTag -> throwEmpty "Int" $ fromValue <$> parseInt inp
    NullTag -> pure $ fromValue Null
    BoolTag -> throwEmpty "Bool" $ fromValue <$> parseBool inp
    UriTag s -> throwEmpty "Any" $ Node (schemaTag s) <$> parseMulti inp
    NoTag -> throwEmpty "Any" $ fromValue <$> parseMulti inp
    _ -> throwError $ InvalidScalarTag tg inp

  parseBool "true" = pure $ Bool True
  parseBool "false" = pure $ Bool False
  parseBool _ = empty

  parseStr s = pure $ String (decodeUtf8 s)

  parseInt s = Integer <$> parseRead s

  parseFloat s = Number <$> parseRead s

  parseMulti s =
    parseInt s <|> parseFloat s <|> parseBool s <|> parseStr s

  throwEmpty :: (Error YamlError :> es) => String -> Eff (NonDet : es) a -> Eff es a
  throwEmpty expt eff = do
    ec <- runNonDet OnEmptyKeep eff
    case ec of
      Left _ -> throwError $ InvalidScalar expt tg inp
      Right a -> pure a

  parseRead :: (Read a, NonDet :> es) => ByteString -> Eff es a
  parseRead s = do
    maybe empty pure $ readMaybe (unpack $ decodeUtf8 s)


-- | Await an event. Throw if out of input
event :: (Error YamlError :> es) => ConduitT i o (Eff es) i
event = do
  e <- await
  case e of
    Nothing -> lift $ throwError NoInput
    Just a -> pure a


-- rootNode :: (Error YamlError :> es) => ConduitT Node Void (Eff es) Node
-- rootNode = do
--   mv <- C.head
--   case mv of
--     Nothing -> fail "Missing Root Value"
--     Just v -> pure v
--
tag :: Tag -> SchemaTag
tag (UriTag s) =
  let t = pack s
      mt = T.stripPrefix "tag:stsci.edu:asdf/" t
   in SchemaTag (maybe (pure t) pure mt)
tag _ = mempty


isNDArray :: SchemaTag -> Bool
isNDArray (SchemaTag Nothing) = False
isNDArray (SchemaTag (Just t)) =
  "core/ndarray" `T.isPrefixOf` t


expect :: (Error YamlError :> es) => Event -> ConduitT Event o (Eff es) ()
expect ex = do
  e <- event
  if e == ex
    then pure ()
    else lift $ throwError $ ExpectedEvent ("Exactly " ++ show ex) e


data YamlError
  = NoInput
  | ExpectedEvent String Event
  | InvalidScalar String Tag ByteString
  | InvalidScalarTag Tag ByteString
  | InvalidTree String Value
  | NDArrayMissingKey String
  | NDArrayMissingBlock Integer
  | NDArrayExpected String Value
  deriving (Show)


testDecode :: IO ()
testDecode = do
  inp <- BS.readFile "samples/example.asdf"
  a <- runEff $ runErrorNoCallStackWith @AsdfError throwM $ do
    decode @Asdf inp
  print a.history
  print a.library
  print a.tree