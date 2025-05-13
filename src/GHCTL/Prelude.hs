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
  , decodeYamlOrDie
  , fromJSONOrDie
  , fromRightOrDie
  , logErrorDie
  ) where

import Autodocodec (HasCodec, parseJSONViaCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Blammo.Logging as X
import Data.Aeson (Value)
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Aeson.Types qualified as Aeson
import Data.List as X (elem)
import Data.Text as X (pack, unpack)
import Data.Text qualified as T
import Data.These as X
import Data.Traversable as X (for)
import Data.Yaml qualified as Yaml
import Path as X (Abs, Dir, File, Path, Rel, toFilePath, (<.>), (</>))
import Relude as X hiding (elem)
import UnliftIO as X (MonadUnliftIO)
import UnliftIO.Exception as X (catch, handle, throwIO)

decodeYamlOrDie
  :: forall m a b
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Path b File
  -> m a
decodeYamlOrDie path = do
  bytes <- readFileBS $ toFilePath path
  fromRightOrDie err $ eitherDecodeYamlViaCodec bytes
 where
  err ex = pack (Yaml.prettyPrintParseException ex) :# []

fromJSONOrDie
  :: forall m a
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Value
  -> m a
fromJSONOrDie value =
  fromRightOrDie err $ Aeson.parseEither parseJSONViaCodec value
 where
  err ex = pack ex :# ["input" .= value]

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
