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
  , sourceChanges
  ) where

import GHCTL.Prelude

import Data.These.Combinators (bimapThese)
import GHCTL.BranchProtection
import GHCTL.Conduit
import GHCTL.RepositoriesYaml
import GHCTL.Repository
import GHCTL.Ruleset

data Change
  = ChangeRepository (Attribute Repository)
  | ChangeBranchProtection (Attribute BranchProtection)
  | ChangeRuleset (Attribute Ruleset)
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON) -- logging

data Attribute a = Attribute
  { repository :: Repository
  , desiredCurrent :: These a a
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON) -- logging

sourceChanges
  :: Monad m
  => [RepositoriesYaml]
  -> [RepositoriesYaml]
  -> ConduitT () Change m ()
sourceChanges as bs =
  yield (These as bs)
    .| pairTheseOnC (.repository.full_name)
    .| with
      getRepository
      ( \repository ->
          sequenceSinks_
            [ bothTheseC (.repository)
                .| filterTheseC (/=)
                .| mapC (ChangeRepository . Attribute repository)
            , bothTheseC (.branch_protection)
                .| catTheseMaybesC
                .| filterTheseC (/=)
                .| mapC (ChangeBranchProtection . Attribute repository)
            , bothTheseC (.rulesets)
                .| pairTheseOnC (.name)
                .| filterTheseC (/=)
                .| mapC (ChangeRuleset . Attribute repository)
            ]
      )

getRepository :: These RepositoriesYaml RepositoriesYaml -> Repository
getRepository = mergeThese const . bimapThese (.repository) (.repository)

with :: Monad m => (i -> a) -> (a -> ConduitT i o m ()) -> ConduitT i o m ()
with f g = awaitForever $ \i -> g $ f i
