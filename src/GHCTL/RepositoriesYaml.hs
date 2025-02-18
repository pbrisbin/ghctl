-- |
--
-- Module      : GHCTL.RepositoriesYaml
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoriesYaml
  ( RepositoryYaml (..)
  , getDesiredRepositoriesYaml
  , getCurrentRepositoryYaml
  ) where

import GHCTL.Prelude

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml qualified as Yaml
import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.KeyedList
import GHCTL.PathArg
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable

data RepositoryYaml = RepositoryYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: KeyedList "name" Ruleset
  , variables :: KeyedList "name" Variable
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RepositoriesYaml = RepositoriesYaml
  { defaults :: Maybe Value
  , repositories :: [Value]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getDesiredRepositoriesYaml
  :: (MonadIO m, MonadLogger m) => PathArg -> m [RepositoryYaml]
getDesiredRepositoriesYaml pathArg = do
  bytes <- getPathArgBytes pathArg
  RepositoriesYaml {defaults, repositories} <- decodeYamlOrDie path bytes
  traverse (fromJSONOrDie path . maybe id overwriteValues defaults) repositories
 where
  path = showPathArg pathArg

getCurrentRepositoryYaml
  :: MonadGitHub m => RepositoryFullName -> m (Maybe RepositoryYaml)
getCurrentRepositoryYaml name = do
  mRepo <- GitHub.getRepository name.owner name.name

  for mRepo $ \repository -> do
    branch_protection <-
      GitHub.getBranchProtection
        name.owner
        name.name
        repository.default_branch

    rulesets <- fmap KeyedList $ do
      rs <- GitHub.getAllRepositoryRulesets name.owner name.name
      traverse (GitHub.getRepositoryRuleset name.owner name.name . (.id)) rs

    variables <-
      KeyedList . (.variables) <$> GitHub.listRepositoryVariables name.owner name.name

    pure RepositoryYaml {repository, branch_protection, rulesets, variables}

decodeYamlOrDie
  :: forall m a
   . (FromJSON a, MonadIO m, MonadLogger m)
  => String
  -- ^ For error message
  -> ByteString
  -> m a
decodeYamlOrDie path bytes =
  case Yaml.decodeEither' bytes of
    Left ex -> do
      let
        message :: Text
        message =
          "Exception decoding repositories file:\n"
            <> pack (Yaml.prettyPrintParseException ex)
      logError $ message :# ["path" .= path]
      exitFailure
    Right a -> pure a

fromJSONOrDie
  :: forall m a
   . (FromJSON a, MonadIO m, MonadLogger m)
  => String
  -- ^ For error message
  -> Value
  -> m a
fromJSONOrDie path value =
  case fromJSON value of
    Error err -> do
      logError $ pack err :# ["path" .= path, "input" .= value]
      exitFailure
    Success a -> pure a

overwriteValues :: Value -> Value -> Value
overwriteValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith overwriteValues a b
  (_, v) -> v
