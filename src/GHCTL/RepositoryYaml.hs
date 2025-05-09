-- |
--
-- Module      : GHCTL.RepositoriesYaml
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoryYaml
  ( RepositoryYaml (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec
import GHCTL.BranchProtection
import GHCTL.KeyedList
import GHCTL.Repository
import GHCTL.Ruleset
import GHCTL.Variable

data RepositoryYaml = RepositoryYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: KeyedList "name" Ruleset
  , variables :: KeyedList "name" Variable
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RepositoryYaml)

instance HasCodec RepositoryYaml where
  codec =
    object "RepositoryYaml"
      $ RepositoryYaml
      <$> (requiredField' "repository" .= (.repository))
      <*> (optionalFieldOrNull' "branch_protection" .= (.branch_protection))
      <*> (requiredField' "rulesets" .= (.rulesets))
      <*> (requiredField' "variables" .= (.variables))
