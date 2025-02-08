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
import GHCTL.GitHub (MonadGitHub (..))
import GHCTL.GitHub.Client
import GHCTL.GitHub.Token

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

instance
  (HasGitHubToken env, HasLogger env, MonadIO m)
  => MonadGitHub (ActualGitHub env m)
  where
  getRepository owner name =
    getGitHubMaybe $ "/repos/" <> owner <> "/" <> name
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
