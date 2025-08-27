-- |
--
-- Module      : GHCTL.Repository
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Repository
  ( Repository (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec

data Repository = Repository
  { description :: Maybe Text
  , private :: Bool
  , has_issues :: Bool
  , has_projects :: Bool
  , has_wiki :: Bool
  , archived :: Bool
  , fork :: Bool
  , default_branch :: Text
  , allow_squash_merge :: Bool
  , allow_merge_commit :: Bool
  , allow_rebase_merge :: Bool
  , allow_auto_merge :: Bool
  , delete_branch_on_merge :: Bool
  , allow_update_branch :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Repository)

instance HasCodec Repository where
  codec =
    object "Repository"
      $ Repository
      <$> (optionalFieldOrNull' "description" .= (.description))
      <*> (requiredField' "private" .= (.private))
      <*> (requiredField' "has_issues" .= (.has_issues))
      <*> (requiredField' "has_projects" .= (.has_projects))
      <*> (requiredField' "has_wiki" .= (.has_wiki))
      <*> (requiredField' "archived" .= (.archived))
      <*> (requiredField' "fork" .= (.fork))
      <*> (requiredField' "default_branch" .= (.default_branch))
      <*> (requiredField' "allow_squash_merge" .= (.allow_squash_merge))
      <*> (requiredField' "allow_merge_commit" .= (.allow_merge_commit))
      <*> (requiredField' "allow_rebase_merge" .= (.allow_rebase_merge))
      <*> (requiredField' "allow_auto_merge" .= (.allow_auto_merge))
      <*> (requiredField' "delete_branch_on_merge" .= (.delete_branch_on_merge))
      <*> (requiredField' "allow_update_branch" .= (.allow_update_branch))
