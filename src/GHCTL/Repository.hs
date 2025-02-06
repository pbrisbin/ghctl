module GHCTL.Repository
  ( Repository (..)
  ) where

import GHCTL.Prelude

import GHCTL.RepositoryFullName

data Repository = Repository
  { full_name :: RepositoryFullName
  , description :: Maybe Text
  , private :: Bool
  , has_issues :: Bool
  , has_projects :: Bool
  , has_wiki :: Bool
  , default_branch :: Text
  , allow_squash_merge :: Bool
  , allow_merge_commit :: Bool
  , allow_rebase_merge :: Bool
  , allow_auto_merge :: Bool
  , delete_branch_on_merge :: Bool
  , allow_update_branch :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
