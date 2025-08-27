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

  createUserRepository :: Text -> Repository -> m ()

  createOrgRepository :: Text -> Text -> Repository -> m ()

  getRepository :: Text -> Text -> m (Maybe Repository)

  listUserRepositories :: Text -> m [Identified]

  updateRepository :: Text -> Text -> Repository -> m ()

  deleteRepository :: Text -> Text -> m ()

  getBranchProtection :: Text -> Text -> Text -> m (Maybe BranchProtection)

  createRepositoryRuleset :: Text -> Text -> Ruleset -> m ()

  listRepositoryRulesets :: Text -> Text -> m [Identified]

  getRepositoryRuleset :: Text -> Text -> Int -> m Ruleset

  updateRepositoryRuleset :: Text -> Text -> Int -> Ruleset -> m ()

  createRepositoryVariable :: Text -> Text -> Variable -> m ()

  listRepositoryVariables :: Text -> Text -> m Variables
