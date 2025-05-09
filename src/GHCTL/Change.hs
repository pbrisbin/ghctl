-- |
--
-- Module      : GHCTL.Change
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change
  ( Change (..)
  , Attribute (..)
  ) where

import GHCTL.Prelude

import GHCTL.BranchProtection
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable

data Change
  = ChangeRepository (Attribute Repository)
  | ChangeBranchProtection (Attribute BranchProtection)
  | ChangeRuleset (Attribute Ruleset)
  | ChangeVariable (Attribute Variable)

data Attribute a = Attribute
  { repository :: RepositoryFullName
  , desiredCurrent :: These a a
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON) -- logging
