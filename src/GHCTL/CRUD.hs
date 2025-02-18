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
  create :: Repository -> a -> m ()
  update :: Repository -> a -> a -> m ()
  delete :: Repository -> a -> m ()

instance MonadGitHub m => HasCRUD Repository m where
  create = error "unimplemented"

  update _ desired _ =
    GitHub.updateRepository
      desired.full_name.owner
      desired.full_name.name
      desired

  delete = error "unimplemented"

instance HasCRUD BranchProtection m where
  create = error "unimplemented"
  update = error "unimplemented"
  delete = error "unimplemented"

instance (MonadGitHub m, MonadLogger m) => HasCRUD Ruleset m where
  create repository =
    GitHub.createRepositoryRuleset
      repository.full_name.owner
      repository.full_name.name

  update repository desired current = do
    mrid <-
      GitHub.getRepositoryRulesetIdByName
        repository.full_name.owner
        repository.full_name.name
        current.name

    case mrid of
      Nothing ->
        logWarn
          $ "Cannot apply UpdateRuleset"
          :# [ "reason" .= ("no ruleset with given name" :: Text)
             , "repository" .= repository.full_name
             , "name" .= current.name
             ]
      Just rid ->
        GitHub.updateRepositoryRuleset
          repository.full_name.owner
          repository.full_name.name
          rid
          desired

  delete = error "unimplemented"

instance MonadGitHub m => HasCRUD Variable m where
  create repository =
    GitHub.createRepositoryVariable
      repository.full_name.owner
      repository.full_name.name

  update = error "unimplemented"
  delete = error "unimplemented"
