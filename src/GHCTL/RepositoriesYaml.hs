-- |
--
-- Module      : GHCTL.RepositoriesYaml
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoriesYaml
  ( RepositoriesYaml (..)
  , getDesiredRepositoriesYaml
  , getCurrentRepositoriesYaml
  ) where

import GHCTL.Prelude

import Data.Aeson
import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.KeyedList
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable
import GHCTL.Yaml qualified as Yaml
import Path (relfile, (</>))

data RepositoriesYaml = RepositoriesYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: KeyedList "name" Ruleset
  , variables :: KeyedList "name" Variable
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getDesiredRepositoriesYaml
  :: (MonadIO m, MonadLogger m) => Path b Dir -> m [RepositoriesYaml]
getDesiredRepositoriesYaml dir = do
  defaults <- Yaml.decodeOptionalFile (object []) [relfile|defaults.yaml|]
  Yaml.decodeAllDefaults defaults $ dir </> [relfile|repositories.yaml|]

getCurrentRepositoriesYaml
  :: MonadGitHub m => [RepositoryFullName] -> m [RepositoriesYaml]
getCurrentRepositoriesYaml = foldMapM getCurrent

getCurrent :: MonadGitHub m => RepositoryFullName -> m [RepositoriesYaml]
getCurrent name = do
  mRepo <- GitHub.getRepository name.owner name.name

  fmap maybeToList $ for mRepo $ \repository -> do
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

    pure RepositoriesYaml {repository, branch_protection, rulesets, variables}
