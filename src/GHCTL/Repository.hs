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
      <*> (optionalFieldWithDefault' "has_issues" True .= (.has_issues))
      <*> (optionalFieldWithDefault' "has_projects" True .= (.has_projects))
      <*> (optionalFieldWithDefault' "has_wiki" True .= (.has_wiki))
      <*> (optionalFieldWithDefault' "archived" False .= (.archived))
      <*> (optionalFieldWithDefault' "fork" False .= (.fork))
      <*> (optionalFieldWithDefault' "default_branch" "main" .= (.default_branch))
      <*> (optionalFieldWithDefault' "allow_squash_merge" True .= (.allow_squash_merge))
      <*> (optionalFieldWithDefault' "allow_merge_commit" True .= (.allow_merge_commit))
      <*> (optionalFieldWithDefault' "allow_rebase_merge" True .= (.allow_rebase_merge))
      <*> (optionalFieldWithDefault' "allow_auto_merge" True .= (.allow_auto_merge))
      <*> ( optionalFieldWithDefault' "delete_branch_on_merge" False
              .= (.delete_branch_on_merge)
          )
      <*> (optionalFieldWithDefault' "allow_update_branch" False .= (.allow_update_branch))
