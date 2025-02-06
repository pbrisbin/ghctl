-- |
--
-- Module      : GHCTL.Change.Apply
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change.Apply
  ( applyChange
  ) where

import GHCTL.Prelude

import GHCTL.Change
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset

applyChange :: (MonadGitHub m, MonadLogger m) => Change -> m ()
applyChange = \case
  CreateRepository {} -> logWarn "unimplemented"
  DeleteRepository {} -> logWarn "unimplemented"
  UpdateRepository desired _ ->
    GitHub.updateRepository
      desired.full_name.owner
      desired.full_name.name
      desired
  CreateBranchProtection {} -> logWarn "unimplemented"
  DeleteBranchProtection {} -> logWarn "unimplemented"
  UpdateBranchProtection {} -> logWarn "unimplemented"
  CreateRuleset repository ruleset ->
    GitHub.createRepositoryRuleset
      repository.full_name.owner
      repository.full_name.name
      ruleset
  DeleteRuleset {} -> logWarn "unimplemented"
  UpdateRuleset repository desired current -> do
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
