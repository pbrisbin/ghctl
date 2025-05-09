-- |
--
-- Module      : GHCTL.RepositoryYaml.Fetch
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoryYaml.Fetch
  ( getCurrentRepositoryYaml
  ) where

import GHCTL.Prelude

import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.KeyedList
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import GHCTL.Variable

-- | Fetch current GitHub state as a 'RepositoryYaml'
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
      rs <- GitHub.listRepositoryRulesets name.owner name.name
      traverse (GitHub.getRepositoryRuleset name.owner name.name . (.id)) rs

    variables <-
      KeyedList . (.variables) <$> GitHub.listRepositoryVariables name.owner name.name

    pure RepositoryYaml {repository, branch_protection, rulesets, variables}
