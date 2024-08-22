module Telescope.Asdf.Error where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Effectful
import Effectful.Error.Static


data AsdfError
  = YamlError String
  | BlockError String
  | ParseError String
  | EncodeError String
  deriving (Exception, Eq)


instance Show AsdfError where
  show (YamlError s) = "YamlError " ++ s
  show (BlockError s) = "BlockError " ++ s
  show (ParseError s) = "ParseError " ++ s
  show (EncodeError s) = "EncodeError " ++ s


expected :: (Show a) => String -> a -> String
expected ex n = "Expected " ++ ex ++ ", but got: " ++ show n


runAsdfM :: (MonadIO m, MonadThrow m) => Eff [Error AsdfError, IOE] a -> m a
runAsdfM = liftIO . runEff . runErrorNoCallStackWith @AsdfError throwM
