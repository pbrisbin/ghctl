-- |
--
-- Module      : GHCTL.GitHub
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.GitHub
  ( Identified (..)
  , MonadGitHub (..)
  ) where

import GHCTL.Prelude

import GHCTL.BranchProtection
import GHCTL.Identified
import GHCTL.Repository
import GHCTL.Ruleset
import GHCTL.User
import GHCTL.Variable

class Monad m => MonadGitHub m where
  getUser :: m User

  getRepository :: Text -> Text -> m (Maybe Repository)

  createUserRepository :: Repository -> m ()

  createOrgRepository :: Repository -> m ()

  getBranchProtection :: Text -> Text -> Text -> m (Maybe BranchProtection)

  getAllRepositoryRulesets :: Text -> Text -> m [Identified]

  getRepositoryRuleset :: Text -> Text -> Int -> m Ruleset

  getRepositoryRulesetIdByName :: Text -> Text -> Text -> m (Maybe Int)
  getRepositoryRulesetIdByName owner repo name = do
    rulesets <- getAllRepositoryRulesets owner repo
    pure $ (.id) <$> find ((== name) . (.name)) rulesets

  listRepositoryVariables :: Text -> Text -> m Variables

  updateRepository :: Text -> Text -> Repository -> m ()

  updateRepositoryRuleset :: Text -> Text -> Int -> Ruleset -> m ()

  createRepositoryRuleset :: Text -> Text -> Ruleset -> m ()

  createRepositoryVariable :: Text -> Text -> Variable -> m ()
