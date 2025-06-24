module Telescope.Fits.Header.Class where

import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Effectful
import GHC.Generics
import Telescope.Data.Axes (AxisOrder (..))
import Telescope.Data.KnownText
import Telescope.Data.Parser
import Telescope.Data.WCS (CType (..), CUnit (..), WCSAxis (..), toWCSAxisKey)
import Telescope.Fits.Header.Header (Header (..), HeaderRecord (..), lookupKeyword)
import Telescope.Fits.Header.Keyword
import Telescope.Fits.Header.Value
import Text.Casing (fromHumps, toSnake)


class ToKeyword a where
  toKeywordValue :: a -> Value


  toKeywordRecord :: Text -> a -> KeywordRecord
  default toKeywordRecord :: Text -> a -> KeywordRecord
  toKeywordRecord key a =
    KeywordRecord key (toKeywordValue a) Nothing


class FromKeyword a where
  parseKeywordValue :: (Parser :> es) => Value -> Eff es a


instance ToKeyword Int where
  toKeywordValue = Integer
instance FromKeyword Int where
  parseKeywordValue = \case
    Integer n -> pure n
    v -> expected "Integer" v


instance ToKeyword Float where
  toKeywordValue = Float . realToFrac
instance FromKeyword Float where
  parseKeywordValue = \case
    Float n -> pure $ realToFrac n
    v -> expected "Float" v


instance ToKeyword Double where
  toKeywordValue = Float
instance FromKeyword Double where
  parseKeywordValue = \case
    Float n -> pure n
    v -> expected "Double" v


instance ToKeyword Text where
  toKeywordValue = String
instance FromKeyword Text where
  parseKeywordValue = \case
    String n -> pure n
    v -> expected "String" v


instance ToKeyword Bool where
  toKeywordValue True = Logic T
  toKeywordValue False = Logic F
instance FromKeyword Bool where
  parseKeywordValue = \case
    Logic c -> pure $ c == T
    v -> expected "Logic" v


instance ToKeyword UTCTime where
  toKeywordValue utc = String $ pack $ iso8601Show utc
instance FromKeyword UTCTime where
  parseKeywordValue = \case
    String t -> do
      case iso8601ParseM $ unpack t of
        Nothing -> expected "UTCTime" t
        Just utc -> pure utc
    v -> expected "UTCTime" v


instance ToKeyword CUnit where
  toKeywordValue (CUnit t) = toKeywordValue t
instance FromKeyword CUnit where
  parseKeywordValue = \case
    String t -> pure $ CUnit t
    v -> expected "CUnit" v


instance ToKeyword CType where
  toKeywordValue (CType t) = toKeywordValue t
instance FromKeyword CType where
  parseKeywordValue = \case
    String t -> pure $ CType t
    v -> expected "CType" v


class ToHeader a where
  toHeader :: a -> Header
  default toHeader :: (Generic a, GToHeader (Rep a)) => a -> Header
  toHeader = gToHeader . from


instance (ToHeader a) => ToHeader (Maybe a) where
  toHeader Nothing = mempty
  toHeader (Just a) = toHeader a


instance (ToHeader a) => ToHeader [a] where
  toHeader = mconcat . fmap toHeader


instance ToHeader Header where
  toHeader = id


instance ToHeader [HeaderRecord] where
  toHeader = Header


instance (AxisOrder ax, KnownText alt) => ToHeader (WCSAxis alt ax) where
  toHeader axis =
    mconcat
      [ axisKey "ctype" axis.ctype
      , axisKey "cunit" axis.cunit
      , axisKey "crpix" axis.crpix
      , axisKey "crval" axis.crval
      , axisKey "cdelt" axis.cdelt
      ]
   where
    axisKey :: (ToKeyword a) => String -> a -> Header
    axisKey s a =
      Header [Keyword $ toKeywordRecord (keyword s) a]

    keyword s = toWCSAxisKey @alt @ax $ cleanKeyword s


class FromHeader a where
  parseHeader :: (Parser :> es) => Header -> Eff es a
  default parseHeader :: (Generic a, GFromHeader (Rep a), Parser :> es) => Header -> Eff es a
  parseHeader h = to <$> gParseHeader h


instance FromHeader Header where
  parseHeader = pure


instance FromHeader [HeaderRecord] where
  parseHeader h = pure h.records


instance (AxisOrder ax, KnownText alt) => FromHeader (WCSAxis alt ax) where
  parseHeader h = do
    ctype <- parseAxisKey "ctype" h
    cunit <- parseAxisKey "cunit" h
    crpix <- parseAxisKey "crpix" h
    crval <- parseAxisKey "crval" h
    cdelt <- parseAxisKey "cdelt" h
    pure $ WCSAxis{ctype, cunit, crpix, crval, cdelt}
   where
    parseAxisKey :: (FromKeyword a, Parser :> es) => String -> Header -> Eff es a
    parseAxisKey k = do
      parseKeyword (toWCSAxisKey @alt @ax $ cleanKeyword k)


parseKeyword :: (FromKeyword a, Parser :> es) => Text -> Header -> Eff es a
parseKeyword k h =
  case lookupKeyword k h of
    Nothing -> parseFail $ "Missing key: " ++ show k
    Just v -> parseAt (Child k) $ parseKeywordValue v


class GToHeader f where
  gToHeader :: f p -> Header


instance (GToHeader f) => GToHeader (M1 D c f) where
  gToHeader (M1 f) = gToHeader f


instance (GToHeader f) => GToHeader (M1 C c f) where
  gToHeader (M1 f) = gToHeader f


instance (GToHeader f, GToHeader g) => GToHeader (f :*: g) where
  gToHeader (f :*: g) = gToHeader f <> gToHeader g


instance {-# OVERLAPPABLE #-} (ToKeyword a, Selector s) => GToHeader (M1 S s (K1 R a)) where
  gToHeader (M1 (K1 a)) = keywordForField (selName (undefined :: M1 S s f p)) a


instance {-# OVERLAPS #-} (ToKeyword a, Selector s) => GToHeader (M1 S s (K1 R (Maybe a))) where
  gToHeader (M1 (K1 Nothing)) = Header []
  gToHeader (M1 (K1 (Just a))) = keywordForField (selName (undefined :: M1 S s f p)) a


instance {-# OVERLAPS #-} (ToHeader a, Selector s) => GToHeader (M1 S s (K1 R (HeaderFor a))) where
  gToHeader (M1 (K1 (HeaderFor a))) = toHeader a


class GFromHeader f where
  gParseHeader :: (Parser :> es) => Header -> Eff es (f p)


instance (GFromHeader f) => GFromHeader (M1 D c f) where
  gParseHeader h = M1 <$> gParseHeader h


instance (GFromHeader f) => GFromHeader (M1 C c f) where
  gParseHeader h = M1 <$> gParseHeader h


instance (GFromHeader f, GFromHeader g) => GFromHeader (f :*: g) where
  gParseHeader h = do
    f <- gParseHeader h
    g <- gParseHeader h
    pure $ f :*: g


instance {-# OVERLAPPABLE #-} (FromKeyword a, Selector s) => GFromHeader (M1 S s (K1 R a)) where
  gParseHeader h = do
    let k = cleanKeyword $ selName (undefined :: M1 S s f p)
    M1 . K1 <$> parseKeyword k h


instance {-# OVERLAPS #-} (FromKeyword a, Selector s) => GFromHeader (M1 S s (K1 R (Maybe a))) where
  gParseHeader h = do
    let k = cleanKeyword $ selName (undefined :: M1 S s f p)
    let mval = lookupKeyword k h :: Maybe Value
    M1 . K1 <$> case mval of
      Nothing -> pure Nothing
      Just v -> do
        a <- parseAt (Child k) $ parseKeywordValue v
        pure $ Just a


cleanKeyword :: String -> Text
cleanKeyword = T.toUpper . pack . toSnake . fromHumps


newtype HeaderFor a = HeaderFor a


keywordForField :: (ToKeyword a) => String -> a -> Header
keywordForField selector a =
  Header [Keyword $ toKeywordRecord (cleanKeyword selector) a]
