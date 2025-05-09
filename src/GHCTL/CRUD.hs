-- |
--
-- Module      : GHCTL.CRUD
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.CRUD
  ( HasCRUD (..)
  ) where

import GHCTL.Prelude

import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable

class HasCRUD a m where
  create :: RepositoryFullName -> a -> m ()
  update :: RepositoryFullName -> a -> a -> m ()
  delete :: RepositoryFullName -> a -> m ()

instance MonadGitHub m => HasCRUD Repository m where
  create name = GitHub.createRepository name.owner name.name
  update name desired _ = GitHub.updateRepository name.owner name.name desired
  delete = error "unimplemented"

instance HasCRUD BranchProtection m where
  create = error "unimplemented"
  update = error "unimplemented"
  delete = error "unimplemented"

instance (MonadGitHub m, MonadLogger m) => HasCRUD Ruleset m where
  create name = GitHub.createRepositoryRuleset name.owner name.name
  update name desired current = do
    mrid <-
      GitHub.getRepositoryRulesetIdByName
        name.owner
        name.name
        current.name

    case mrid of
      Nothing ->
        logWarn
          $ "Cannot apply UpdateRuleset"
          :# [ "reason" .= ("no ruleset with given name" :: Text)
             , "repository" .= name
             , "name" .= current.name
             ]
      Just rid ->
        GitHub.updateRepositoryRuleset
          name.owner
          name.name
          rid
          desired

  delete = error "unimplemented"

instance MonadGitHub m => HasCRUD Variable m where
  create name = GitHub.createRepositoryVariable name.owner name.name
  update = error "unimplemented"
  delete = error "unimplemented"
