-- |
--
-- Module      : GHCTL.BranchProtection
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.BranchProtection
  ( BranchProtection (..)
  ) where

import GHCTL.Prelude

import GHCTL.BoolEnabled

data BranchProtection = BranchProtection
  { required_status_checks :: Maybe StatusChecks
  , enforce_admins :: BoolEnabled
  , required_pull_request_reviews :: Maybe PullRequestReviews
  , restrictions :: Maybe UsersTeamsApps
  , required_linear_history :: BoolEnabled
  , allow_force_pushes :: BoolEnabled
  , allow_deletions :: BoolEnabled
  , block_creations :: BoolEnabled
  , required_conversation_resolution :: BoolEnabled
  , lock_branch :: BoolEnabled
  , allow_fork_syncing :: BoolEnabled
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data StatusChecks = StatusChecks
  { strict :: Bool
  , contexts :: [Text]
  , checks :: [StatusCheck]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data StatusCheck = StatusCheck
  { context :: Text
  , app_id :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data PullRequestReviews = PullRequestReviews
  { dismissal_restrictions :: Maybe UsersTeamsApps
  , dismiss_stale_reviews :: Maybe Bool
  , require_code_owner_reviews :: Maybe Bool
  , required_approving_review_count :: Maybe Int
  , require_last_push_approval :: Maybe Bool
  , bypass_pull_request_allowances :: Maybe UsersTeamsApps
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data UsersTeamsApps = UsersTeamsApps
  { users :: [Text]
  , teams :: [Text]
  , apps :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
