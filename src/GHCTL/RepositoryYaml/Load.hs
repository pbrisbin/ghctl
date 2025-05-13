-- |
--
-- Module      : GHCTL.RepositoryYaml.Load
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoryYaml.Load
  ( loadRepositoryYaml
  ) where

import GHCTL.Prelude

import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml

loadRepositoryYaml
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Value
  -- ^ Defaults
  -> Path b Dir
  -- ^ Parent that path is relative to
  -> Path Rel File
  -- ^ Repository path, expected to be @{owner}/{name}.yaml@
  -> m (Map RepositoryFullName (Maybe RepositoryYaml))
loadRepositoryYaml defaults dir path = do
  withThreadContext ["path" .= toFilePath path] $ do
    case repositoryFullNameFromFile path of
      Left err -> do
        logWarn $ "Repository path is invalid, skipping" :# ["error" .= err]
        pure mempty
      Right name -> do
        val <- decodeYamlOrDie $ dir </> path
        yaml <- fromJSONOrDie $ overwriteValues defaults val
        pure $ Map.singleton name $ Just yaml

overwriteValues :: Value -> Value -> Value
overwriteValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith overwriteValues a b
  (_, v) -> v
