module Telescope.Asdf.Error where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Effectful
import Effectful.Error.Static
import Telescope.Asdf.Node


data AsdfError
  = YamlError String
  | BlockError String
  | ParseError String
  | EncodeError String
  | MissingAnchor Anchor Anchors
  deriving (Exception, Eq)


instance Show AsdfError where
  show (YamlError s) = "YamlError " ++ s
  show (BlockError s) = "BlockError " ++ s
  show (ParseError s) = "ParseError " ++ s
  show (EncodeError s) = "EncodeError " ++ s
  show (MissingAnchor a as) = "MissingAnchor " ++ show a ++ " in " ++ show as


runAsdfM :: (MonadIO m, MonadThrow m) => Eff [Error AsdfError, IOE] a -> m a
runAsdfM = liftIO . runEff . runErrorNoCallStackWith @AsdfError throwM
