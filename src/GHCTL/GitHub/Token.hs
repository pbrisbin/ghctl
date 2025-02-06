-- |
--
-- Module      : GHCTL.GitHub.Token
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.GitHub.Token
  ( GitHubToken (..)
  , envGitHubToken
  , HasGitHubToken (..)
  ) where

import GHCTL.Prelude

import Data.ByteString.Char8 qualified as BS8
import System.Environment (getEnv)

newtype GitHubToken = GitHubToken
  { unwrap :: ByteString
  }

envGitHubToken :: MonadIO m => m GitHubToken
envGitHubToken = liftIO $ GitHubToken . BS8.pack <$> getEnv "GITHUB_TOKEN"

class HasGitHubToken env where
  getGitHubToken :: env -> GitHubToken

instance HasGitHubToken GitHubToken where
  getGitHubToken = id
