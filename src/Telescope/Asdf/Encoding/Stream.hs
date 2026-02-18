module Telescope.Asdf.Encoding.Stream where

import Conduit
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Conduit.Combinators (peek)
import Data.Conduit.Combinators qualified as C
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.NonDet
import Effectful.Reader.Dynamic
import Effectful.Resource
import Effectful.State.Static.Local
import Telescope.Asdf.Class hiding (anchor)
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.NDArray (NDArrayData (..))
import Telescope.Asdf.Node
import Telescope.Data.Axes
import Telescope.Data.Numeric (showScientific)
import Telescope.Data.Parser (runParserPure)
import Text.Libyaml (Event (..), MappingStyle (..), SequenceStyle (..), Style (..), Tag (..))
import Text.Libyaml qualified as Yaml
import Text.Read (readMaybe)


runStream :: (IOE :> es) => ConduitT () Void (Eff (State Anchors : State [BlockData] : Resource : es)) a -> Eff es (a, [BlockData])
runStream con = do
  runResource . runState @[BlockData] [] . evalState @Anchors mempty . runConduit $ con


runStreamList :: (IOE :> es) => ConduitT () Event (Eff (State Anchors : State [BlockData] : Resource : es)) () -> Eff es [Event]
runStreamList con = do
  (res, _) <- runStream $ con .| sinkList
  pure res


yieldDocumentStream :: (State [BlockData] :> es, IOE :> es) => ConduitT a Event (Eff es) () -> ConduitT a Event (Eff es) ()
yieldDocumentStream content = do
  yield EventStreamStart
  yield EventDocumentStart
  content
  yield EventDocumentEnd
  yield EventStreamEnd


yieldNode :: forall es a. (IOE :> es, State [BlockData] :> es, State Anchors :> es, Error YamlError :> es) => Node -> ConduitT a Event (Eff es) ()
yieldNode node@(Node st anc val) = do
  lift $ addNodeAnchor node
  yieldValue val
 where
  yieldValue = \case
    Object o -> yieldObject o
    Array a -> yieldArray a
    String "" -> yieldEmptyString
    String s -> yieldScalar (T.encodeUtf8 s)
    Integer n -> yieldNum n
    NDArray nd -> yieldNDArray nd
    Bool b -> yieldBool b
    Number n -> yieldScientific n
    Null -> yieldScalar "~"
    Reference r -> yieldObject [("$ref", toNode $ String (pack $ show r))]
    Alias a -> yieldAlias a

  anchor :: Yaml.Anchor
  anchor =
    case anc of
      Nothing -> Nothing
      (Just (Anchor a)) -> pure $ unpack $ T.map cleanNonAllowable a

  cleanNonAllowable c
    | isAlphaNum c = c
    | '_' <- c = c
    | otherwise = '-'

  tag = case st of
    SchemaTag Nothing -> NoTag
    SchemaTag (Just s) -> UriTag (unpack s)

  yieldScalar s = yield $ EventScalar s tag Plain anchor
  yieldEmptyString = yield $ EventScalar "" tag SingleQuoted anchor

  yieldAlias (Anchor a) = do
    _ <- lift $ resolveAnchor (Anchor a)
    yield $ EventAlias (unpack a)

  yieldNum :: (Num n, Show n) => n -> ConduitT a Event (Eff es) ()
  yieldNum n = yieldScalar (T.encodeUtf8 $ pack $ show n)

  yieldScientific :: Scientific -> ConduitT a Event (Eff es) ()
  yieldScientific n = yieldScalar (T.encodeUtf8 $ pack $ showScientific n)

  yieldBool = \case
    True -> yieldScalar "true"
    False -> yieldScalar "false"

  yieldObject :: [(Key, Node)] -> ConduitT a Event (Eff es) ()
  yieldObject o = do
    yield $ EventMappingStart tag blockStyle anchor
    mapM_ yieldMapping o
    yield EventMappingEnd
   where
    blockStyle
      | any (isComplexNode . snd) o = BlockMapping
      | otherwise = FlowMapping

  yieldMapping :: (Key, Node) -> ConduitT a Event (Eff es) ()
  yieldMapping (key, nd) = do
    yield $ EventScalar (T.encodeUtf8 key) NoTag Plain Nothing
    yieldNode nd

  yieldArray :: [Node] -> ConduitT a Event (Eff es) ()
  yieldArray ns = do
    yield $ EventSequenceStart tag seqStyle anchor
    mapM_ yieldNode ns
    yield EventSequenceEnd
   where
    seqStyle
      | any isComplexNode ns = BlockSequence
      | otherwise = FlowSequence

  yieldNDArray :: NDArrayData -> ConduitT a Event (Eff es) ()
  yieldNDArray nd = do
    src <- lift $ addBlock nd.bytes
    yield $ EventMappingStart (UriTag "!core/ndarray-1.0.0") FlowMapping anchor
    yieldMapping ("source", toNode src)
    yieldMapping ("datatype", toNode nd.datatype)
    yieldMapping ("shape", toNode nd.shape)
    yieldMapping ("byteorder", toNode nd.byteorder)
    yield EventMappingEnd


addBlock :: (State [BlockData] :> es) => ByteString -> Eff es BlockSource
addBlock bytes = do
  blocks <- get @[BlockData]
  put $ blocks <> [BlockData bytes]
  pure $ BlockSource $ length blocks


isComplexNode :: Node -> Bool
isComplexNode (Node _ _ val) = isComplex val
 where
  isComplex = \case
    Array _ -> True
    Object _ -> True
    NDArray _ -> True
    _ -> False


sinkTree :: (Error YamlError :> es, State Anchors :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Object
sinkTree = do
  expect EventStreamStart
  expect EventDocumentStart
  Node _ _ v <- sinkNode
  case v of
    Object o -> pure o
    _ -> lift $ throwError $ InvalidTree "Expected Object" v


sinkNode :: (Error YamlError :> es, State Anchors :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Node
sinkNode = do
  e <- event
  node <- sinkByEvent e
  lift $ addNodeAnchor node
  pure node
 where
  sinkByEvent = \case
    EventScalar s t _ a ->
      lift $ parseScalar (fromString <$> a) s t
    EventMappingStart tg _ a -> do
      let stag = parseSchemaTag tg
      maps <- sinkMappings
      val <- lift $ fromMappings stag maps
      pure $ Node stag (fromString <$> a) val
    EventSequenceStart tg _ a -> do
      ns <- sinkSequence
      pure $ Node (parseSchemaTag tg) (fromString <$> a) $ Array ns
    EventAlias a -> do
      val <- lift $ resolveAnchor (Anchor $ pack a)
      pure $ Node mempty Nothing val
    ev -> lift $ throwError $ ExpectedEvent "Not Handled" ev

  fromMappings :: forall es. (Error YamlError :> es, Reader [BlockData] :> es) => SchemaTag -> [(Key, Node)] -> Eff es Value
  fromMappings stag maps = do
    res <- runNonDet OnEmptyKeep (tryNDArray stag maps <|> tryReference maps) :: Eff es (Either CallStack Value)
    case res of
      Left _ -> pure $ Object maps
      Right val -> pure val

  tryNDArray :: (NonDet :> es, Error YamlError :> es, Reader [BlockData] :> es) => SchemaTag -> [(Key, Node)] -> Eff es Value
  tryNDArray stag maps
    | isNDArray stag = NDArray <$> ndArrayDataFromMaps maps
    | otherwise = empty

  tryReference :: (NonDet :> es, Error YamlError :> es) => [(Key, Node)] -> Eff es Value
  tryReference maps = do
    case lookup "$ref" maps of
      Nothing -> empty
      Just (Node _ _ (String s)) -> pure $ Reference $ jsonReference s
      Just (Node _ _ value) -> throwError $ InvalidReference value


resolveAnchor :: (State Anchors :> es, Error YamlError :> es) => Anchor -> Eff es Value
resolveAnchor anc = do
  Anchors anchors <- get @Anchors
  case lookup anc anchors of
    Nothing -> throwError $ AnchorUndefined anc (fmap fst anchors)
    Just v -> pure v


ndArrayDataFromMaps :: forall es. (Error YamlError :> es, Reader [BlockData] :> es) => [(Key, Node)] -> Eff es NDArrayData
ndArrayDataFromMaps maps = do
  bytes <- require "source" >>= findSource
  datatype <- require "datatype" >>= parseDatatype
  byteorder <- require "byteorder" >>= parseByteorder
  shape <- require "shape" >>= parseShape
  pure $ NDArrayData{bytes, datatype, byteorder, shape}
 where
  require key =
    case lookup key maps of
      Nothing -> throwError $ NDArrayMissingKey (unpack key)
      Just (Node _ _ val) -> pure val

  parseDatatype = parseLocal "DataType"
  parseByteorder = parseLocal "ByteOrder"
  parseShape val =
    case val of
      Array ns -> axesRowMajor <$> mapM (parseAxis . (.value)) ns
      _ -> throwError $ NDArrayExpected "Shape" val

  parseAxis val =
    case val of
      Integer n -> pure $ fromIntegral n
      _ -> throwError $ NDArrayExpected "Shape Axis" val

  findSource val =
    case val of
      Integer s -> do
        blocks <- ask
        if fromIntegral s >= length blocks
          then throwError $ NDArrayMissingBlock s
          else do
            let BlockData b = blocks !! fromIntegral s
            pure b
      _ -> throwError $ NDArrayExpected "Source" val

  parseLocal :: (FromAsdf a, Error YamlError :> es) => String -> Value -> Eff es a
  parseLocal expected val = do
    let res = runParserPure . runReader @Anchors mempty $ parseValue val
    case res of
      Left _ -> throwError $ NDArrayExpected expected val
      Right a -> pure a


sinkMapping :: (Error YamlError :> es, Reader [BlockData] :> es, State Anchors :> es) => ConduitT Event o (Eff es) (Key, Node)
sinkMapping = do
  k <- sinkMapKey
  v <- sinkNode
  pure (k, v)
 where
  sinkMapKey =
    event >>= \case
      EventScalar s _ _ _ -> pure $ decodeUtf8 s
      ev -> lift $ throwError $ ExpectedEvent "Scalar Key" ev


-- we don't have to parse this into an object...
sinkMappings :: (Error YamlError :> es, State Anchors :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [(Key, Node)]
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
      pure $ a : as
    else do
      -- consume the one we matched
      C.drop 1
      pure []


sinkSequence :: (Error YamlError :> es, State Anchors :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [Node]
sinkSequence = do
  sinkWhile (/= EventSequenceEnd) sinkNode


parseScalar :: (Error YamlError :> es) => Maybe Anchor -> ByteString -> Yaml.Tag -> Eff es Node
parseScalar ma inp tg = byTag tg
 where
  byTag :: (Error YamlError :> es) => Yaml.Tag -> Eff es Node
  byTag = \case
    UriTag s -> throwEmpty "Any" $ Node (schemaTag s) ma <$> parseMulti inp
    other -> do
      val <- nonUriTagValue other
      pure $ Node mempty ma val

  nonUriTagValue :: (Error YamlError :> es) => Yaml.Tag -> Eff es Value
  nonUriTagValue = \case
    StrTag -> parseStr inp -- always succeeds
    FloatTag -> throwEmpty "Float" $ parseFloat inp
    IntTag -> throwEmpty "Int" $ parseInt inp
    NullTag -> pure Null
    BoolTag -> throwEmpty "Bool" $ parseBool inp
    NoTag -> throwEmpty "Any" $ parseMulti inp
    _ -> throwError $ InvalidScalarTag tg inp

  parseBool "true" = pure $ Bool True
  parseBool "false" = pure $ Bool False
  parseBool _ = empty

  parseStr s = pure $ String (decodeUtf8 s)

  parseInt s = Integer <$> parseRead s

  parseFloat s = Number <$> parseRead s

  parseNull s = do
    case s of
      "~" -> pure Null
      "null" -> pure Null
      _ -> empty

  parseMulti :: (NonDet :> es) => ByteString -> Eff es Value
  parseMulti s = do
    parseInt s <|> parseFloat s <|> parseBool s <|> parseNull s <|> parseStr s

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


parseSchemaTag :: Tag -> SchemaTag
parseSchemaTag (UriTag s) = schemaTag s
parseSchemaTag _ = mempty


isNDArray :: SchemaTag -> Bool
isNDArray (SchemaTag Nothing) = False
isNDArray (SchemaTag (Just t)) =
  "!core/ndarray" `T.isPrefixOf` t


expect :: (Error YamlError :> es) => Event -> ConduitT Event o (Eff es) ()
expect ex = expect' ("Exactly " ++ show ex) (== ex)


expect' :: (Error YamlError :> es) => String -> (Event -> Bool) -> ConduitT Event o (Eff es) ()
expect' ex p = do
  e <- event
  if p e
    then pure ()
    else lift $ throwError $ ExpectedEvent ex e


data YamlError
  = NoInput
  | ExpectedEvent String Event
  | InvalidScalar String Tag ByteString
  | InvalidScalarTag Tag ByteString
  | InvalidTree String Value
  | NDArrayMissingKey String
  | NDArrayMissingBlock Integer
  | NDArrayExpected String Value
  | InvalidReference Value
  | AnchorUndefined Anchor [Anchor]
  deriving (Show)


sinkIndex :: (Error YamlError :> es) => ConduitT Event o (Eff es) BlockIndex
sinkIndex = do
  expect EventStreamStart
  expect EventDocumentStart
  expect' "EventSequenceStart" isSequence
  ns <- sinkWhile (/= EventSequenceEnd) sinkIndexEntry
  expect EventSequenceEnd
  expect EventDocumentEnd
  expect EventStreamEnd
  pure $ BlockIndex ns
 where
  isSequence :: Event -> Bool
  isSequence EventSequenceStart{} = True
  isSequence _ = False

  sinkIndexEntry :: (Error YamlError :> es) => ConduitT Event o (Eff es) Int
  sinkIndexEntry = do
    e <- event
    case e of
      EventScalar s t _ _ -> do
        case readMaybe (unpack $ decodeUtf8 s) of
          Just n -> pure n
          Nothing -> lift $ throwError $ InvalidScalar "Int Index Entry" t s
      _ -> lift $ throwError $ ExpectedEvent "Scalar Int" e


addNodeAnchor :: (State Anchors :> es) => Node -> Eff es ()
addNodeAnchor n =
  case n.anchor of
    Nothing -> pure ()
    Just a -> modify (Anchors [(a, n.value)] <>)
