module GHCTL.BranchProtection
  ( BranchProtection (..)
  ) where

import GHCTL.Prelude

data BranchProtection = BranchProtection
  { required_status_checks :: Maybe StatusChecks
  , enforce_admins :: Bool
  , required_pull_request_reviews :: Maybe PullRequestReviews
  , restrictions :: UsersTeamsApps
  , required_linear_history :: Bool
  , allow_force_pushes :: Maybe Bool
  , allow_deletions :: Bool
  , block_creations :: Bool
  , required_conversation_resolution :: Bool
  , lock_branch :: Bool
  , allow_fork_syncing :: Bool
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
  { dismissal_restrictions :: UsersTeamsApps
  , dismiss_stale_reviews :: Bool
  , require_code_owner_reviews :: Bool
  , required_approving_review_count :: Bool
  , require_last_push_approval :: Bool
  , bypass_pull_request_allowances :: UsersTeamsApps
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
