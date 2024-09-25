module Telescope.Data.Parser where

import Control.Monad.Catch (Exception)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static


data Parser :: Effect where
  ParseFail :: String -> Parser m a
  PathAdd :: Ref -> m a -> Parser m a


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


runPureParser :: Eff '[Parser, Error ParseError] a -> Either ParseError a
runPureParser eff = runPureEff . runErrorNoCallStack @ParseError $ runParser eff


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
    intercalate "/" (fmap show ps)


expected :: (Show value, Parser :> es) => String -> value -> Eff es a
expected ex n =
  parseFail $ "Expected " ++ ex ++ ", but got: " ++ show n


parseFail :: (Parser :> es) => String -> Eff es a
parseFail e = send $ ParseFail e


parseAt :: (Parser :> es) => Ref -> Eff es a -> Eff es a
parseAt p parse = do
  send $ PathAdd p parse
