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

data CreateRepo = CreateRepo
  { name :: Text
  , repository :: Repository
  }

instance ToJSON CreateRepo where
  toJSON CreateRepo {name, repository} =
    case toJSON repository of
      Object km -> Object $ KeyMap.insert "name" (String name) km
      v -> v

instance
  (HasGitHubToken env, HasLogger env, MonadIO m)
  => MonadGitHub (ActualGitHub env m)
  where
  getUser = getGitHub "/user"

  createUserRepository name = postGitHub "/user/repos" . CreateRepo name

  createOrgRepository owner name repo =
    postGitHub ("/orgs/" <> owner <> "/repos") $ CreateRepo name repo

  getRepository owner name =
    getGitHubMaybe $ "/repos/" <> owner <> "/" <> name

  updateRepository owner name = postGitHub $ "/repos/" <> owner <> "/" <> name

  deleteRepository owner name = deleteGitHub $ "/repos/" <> owner <> "/" <> name

  getBranchProtection owner name branch =
    getGitHubMaybe
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/branches/"
      <> branch
      <> "/protection"

  createRepositoryRuleset owner name =
    postGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets"

  listRepositoryRulesets owner name =
    getGitHub $ "/repos/" <> owner <> "/" <> name <> "/rulesets"

  getRepositoryRuleset owner name rid =
    getGitHub
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/rulesets/"
      <> show rid

  updateRepositoryRuleset owner name rid =
    putGitHub
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/rulesets/"
      <> show rid

  createRepositoryVariable owner name =
    postGitHub
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/actions/variables"

  listRepositoryVariables owner name =
    getGitHub
      $ "/repos/"
      <> owner
      <> "/"
      <> name
      <> "/actions/variables"
