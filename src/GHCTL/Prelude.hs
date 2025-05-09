-- |
--
-- Module      : GHCTL.Prelude
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Prelude
  ( module X
  , fromRightOrDie
  , logErrorDie
  ) where

import Blammo.Logging as X
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.List as X (elem)
import Data.Text as X (pack, unpack)
import Data.Text qualified as T
import Data.These as X
import Data.Traversable as X (for)
import Path as X (Abs, Dir, File, Path, Rel, toFilePath, (<.>), (</>))
import Relude as X hiding (elem)
import UnliftIO as X (MonadUnliftIO)
import UnliftIO.Exception as X (catch, handle, throwIO)

fromRightOrDie
  :: (MonadIO m, MonadLogger m)
  => (e -> Message) -> Either e a -> m a
fromRightOrDie toMessage = \case
  Left e -> logErrorDie $ toMessage e
  Right a -> pure a

logErrorDie :: (MonadIO m, MonadLogger m) => Message -> m a
logErrorDie (msg :# attrs) = do
  logError $ reindent msg :# attrs
  exitFailure
 where
  reindent = T.intercalate "\n       " . lines
