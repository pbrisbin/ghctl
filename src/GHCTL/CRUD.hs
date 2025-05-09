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
import GHCTL.User qualified as GitHub
import GHCTL.Variable

class HasCRUD a m where
  create :: RepositoryFullName -> a -> m ()
  update :: RepositoryFullName -> a -> a -> m ()
  delete :: RepositoryFullName -> a -> m ()

instance MonadGitHub m => HasCRUD Repository m where
  create = createRepository
  update name desired _ = GitHub.updateRepository name.owner name.name desired
  delete name _ = GitHub.deleteRepository name.owner name.name

instance HasCRUD BranchProtection m where
  create = error "unimplemented"
  update = error "unimplemented"
  delete = error "unimplemented"

instance (MonadGitHub m, MonadLogger m) => HasCRUD Ruleset m where
  create name = GitHub.createRepositoryRuleset name.owner name.name
  update = updateRuleset
  delete = error "unimplemented"

instance MonadGitHub m => HasCRUD Variable m where
  create name = GitHub.createRepositoryVariable name.owner name.name
  update = error "unimplemented"
  delete = error "unimplemented"

createRepository :: MonadGitHub m => RepositoryFullName -> Repository -> m ()
createRepository name repo = do
  GitHub.User {login} <- GitHub.getUser

  if login == name.owner
    then GitHub.createUserRepository name.name repo
    else GitHub.createOrgRepository name.owner name.name repo

updateRuleset
  :: (MonadGitHub m, MonadLogger m)
  => RepositoryFullName
  -> Ruleset
  -> Ruleset
  -> m ()
updateRuleset repo desired current = do
  mRuleset <- getRulesetByName repo current.name

  case mRuleset of
    Nothing ->
      logWarn
        $ "Cannot apply UpdateRuleset"
        :# [ "reason" .= ("no ruleset with given name" :: Text)
           , "repository" .= repo
           , "name" .= current.name
           ]
    Just ruleset ->
      GitHub.updateRepositoryRuleset
        repo.owner
        repo.name
        ruleset.id
        desired

getRulesetByName
  :: MonadGitHub m
  => RepositoryFullName
  -> Text
  -> m (Maybe GitHub.Identified)
getRulesetByName repo name =
  find ((== name) . (.name))
    <$> GitHub.listRepositoryRulesets repo.owner repo.name
