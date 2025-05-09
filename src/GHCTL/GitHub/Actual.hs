-- |
--
-- Module      : GHCTL.GitHub.Actual
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.GitHub.Actual
  ( ActualGitHub (..)
  ) where

import GHCTL.Prelude

import Blammo.Logging.Logger
import Blammo.Logging.WithLogger
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import GHCTL.GitHub (MonadGitHub (..))
import GHCTL.GitHub.Client
import GHCTL.GitHub.Token
import GHCTL.Repository
import GHCTL.RepositoryFullName

newtype ActualGitHub env m a = ActualGitHub
  { unwrap :: ReaderT env m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader env
    )
  deriving (MonadLogger) via (WithLogger env m)
  deriving (MonadLoggerIO) via (WithLogger env m)

newtype CreateRepo = CreateRepo
  { unwrap :: Repository
  }

instance ToJSON CreateRepo where
  toJSON cr =
    let
      repo = cr.unwrap
      name = String repo.full_name.name
    in
      case toJSON repo of
        Object km -> Object $ KeyMap.insert "name" name km
        v -> v

instance
  (HasGitHubToken env, HasLogger env, MonadIO m)
  => MonadGitHub (ActualGitHub env m)
  where
  getUser = getGitHub "/user"
  getRepository owner name =
    getGitHubMaybe $ "/repos/" <> owner <> "/" <> name
  createUserRepository = postGitHub "/user/repos" . CreateRepo
  createOrgRepository repo =
    postGitHub ("/orgs/" <> repo.full_name.owner <> "/repos") $ CreateRepo repo
  getBranchProtection owner name branch =
    getGitHubMaybe
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/branches/"
      <> branch
      <> "/protection"
  getAllRepositoryRulesets owner name =
    getGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets"
  getRepositoryRuleset owner name rid =
    getGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets/" <> show rid
  listRepositoryVariables owner name =
    getGitHub $ "/repos/" <> owner <> "/" <> name <> "/actions/variables"
  updateRepository owner name =
    postGitHub $ "/repos/" <> owner <> "/" <> name
  updateRepositoryRuleset owner name rid =
    putGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets/" <> show rid
  createRepositoryRuleset owner name =
    postGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets"
  createRepositoryVariable owner name =
    postGitHub $ "/repos/" <> owner <> "/" <> name <> "/actions/variables"
