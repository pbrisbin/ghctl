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

import GHCTL.Prelude hiding ((.=))

import Autodocodec
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
  deriving (FromJSON, ToJSON) via (Autodocodec BranchProtection)

instance HasCodec BranchProtection where
  codec =
    object "BranchProtection"
      $ BranchProtection
      <$> (optionalField' "required_status_checks" .= (.required_status_checks))
      <*> (requiredField' "enforce_admins" .= (.enforce_admins))
      <*> ( optionalField' "required_pull_request_reviews"
              .= (.required_pull_request_reviews)
          )
      <*> (optionalField' "restrictions" .= (.restrictions))
      <*> (requiredField' "required_linear_history" .= (.required_linear_history))
      <*> (requiredField' "allow_force_pushes" .= (.allow_force_pushes))
      <*> (requiredField' "allow_deletions" .= (.allow_deletions))
      <*> (requiredField' "block_creations" .= (.block_creations))
      <*> ( requiredField' "required_conversation_resolution"
              .= (.required_conversation_resolution)
          )
      <*> (requiredField' "lock_branch" .= (.lock_branch))
      <*> (requiredField' "allow_fork_syncing" .= (.allow_fork_syncing))

data StatusChecks = StatusChecks
  { strict :: Bool
  , contexts :: [Text]
  , checks :: [StatusCheck]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec StatusChecks)

instance HasCodec StatusChecks where
  codec =
    object "StatusChecks"
      $ StatusChecks
      <$> (requiredField' "strict" .= (.strict))
      <*> (requiredField' "contexts" .= (.contexts))
      <*> (requiredField' "checks" .= (.checks))

data StatusCheck = StatusCheck
  { context :: Text
  , app_id :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec StatusCheck)

instance HasCodec StatusCheck where
  codec =
    object "StatusCheck"
      $ StatusCheck
      <$> (requiredField' "context" .= (.context))
      <*> (optionalField' "app_id" .= (.app_id))

data PullRequestReviews = PullRequestReviews
  { dismissal_restrictions :: Maybe UsersTeamsApps
  , dismiss_stale_reviews :: Maybe Bool
  , require_code_owner_reviews :: Maybe Bool
  , required_approving_review_count :: Maybe Int
  , require_last_push_approval :: Maybe Bool
  , bypass_pull_request_allowances :: Maybe UsersTeamsApps
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec PullRequestReviews)

instance HasCodec PullRequestReviews where
  codec =
    object "PullRequestReviews"
      $ PullRequestReviews
      <$> (optionalField' "dismissal_restrictions" .= (.dismissal_restrictions))
      <*> (optionalField' "dismiss_stale_reviews" .= (.dismiss_stale_reviews))
      <*> (optionalField' "require_code_owner_reviews" .= (.require_code_owner_reviews))
      <*> ( optionalField' "required_approving_review_count"
              .= (.required_approving_review_count)
          )
      <*> (optionalField' "require_last_push_approval" .= (.require_last_push_approval))
      <*> ( optionalField' "bypass_pull_request_allowances"
              .= (.bypass_pull_request_allowances)
          )

data UsersTeamsApps = UsersTeamsApps
  { users :: [Text]
  , teams :: [Text]
  , apps :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec UsersTeamsApps)

instance HasCodec UsersTeamsApps where
  codec =
    object "UsersTeamsApps"
      $ UsersTeamsApps
      <$> (requiredField' "users" .= (.users))
      <*> (requiredField' "teams" .= (.teams))
      <*> (requiredField' "apps" .= (.apps))
