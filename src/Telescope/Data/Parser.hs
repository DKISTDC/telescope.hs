module Telescope.Data.Parser where

import Control.Monad.Catch (Exception)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Fail
import Effectful.Reader.Static


data Parser :: Effect where
  ParseFail :: String -> Parser m a
  PathAdd :: Ref -> m a -> Parser m a


-- PathGet :: Parser m Path

type instance DispatchOf Parser = 'Dynamic


-- do each of them provide their own?
runParser
  :: (Error ParseError :> es)
  => Eff (Parser : es) a
  -> Eff es a
runParser = reinterpret (runReader @Path mempty) $ \env -> \case
  ParseFail e -> do
    path <- ask @Path
    throwError $ ParseFailure path e
  PathAdd p m -> do
    -- copied from Effectful.Reader.Dynamic
    localSeqUnlift env $ \unlift -> local (<> Path [p]) (unlift m)


-- runPureParseError :: Eff '[Error ParseError] a -> Either ParseError a
-- runPureParseError = runPureEff . runErrorNoCallStack @ParseError

runPureParser :: Eff '[Parser, Error ParseError] a -> Either ParseError a
runPureParser eff = runPureEff . runErrorNoCallStack @ParseError $ runParser eff


-- import Telescope.Asdf.Node

data ParseError
  = ParseFailure Path String
  deriving (Exception, Eq)


instance Show ParseError where
  show (ParseFailure path s) =
    "at " ++ show path ++ "\n ! " ++ s


data Ref
  = Child Text
  | Index Int
  deriving (Eq)


instance Show Ref where
  show (Child c) = unpack c
  show (Index n) = show n


newtype Path = Path [Ref]
  deriving (Eq)
  deriving newtype (Semigroup, Monoid)
instance Show Path where
  show (Path ps) =
    intercalate "." (fmap show ps)


-- runPureParser :: Parser' '[Error ParseError] a -> Either ParseError a
-- runPureParser = runPureEff . runErrorNoCallStack @ParseError . parse

-- parse :: (Error ParseError :> es) => Parser' es a -> Eff es a
-- parse eff = runReader @[PathSegment] mempty $ do
--   ea <- runFail eff
--   case ea of
--     Left e -> do
--       p <- currentPath
--       throwError $ ParseFailure p e
--     Right a -> pure a

addPath :: (Reader [Path] :> es) => Path -> Eff es a -> Eff es a
addPath p = local (p :)


-- context :: Parser [Context]
-- context = Parser ask

expected :: (Show value, Parser :> es) => String -> value -> Eff es a
expected ex n =
  parseFail $ "Expected " ++ ex ++ ", but got: " ++ show n


parseFail :: (Parser :> es) => String -> Eff es a
parseFail e = send $ ParseFail e


parseAt :: (Parser :> es) => Ref -> Eff es a -> Eff es a
parseAt p parse = do
  send $ PathAdd p parse
