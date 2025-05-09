module GHCTL.Change.Source
  ( sourceChanges
  ) where

import GHCTL.Prelude

import GHCTL.Change
import GHCTL.Conduit
import GHCTL.GitHub (MonadGitHub)
import GHCTL.KeyedList
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import GHCTL.Ruleset
import GHCTL.Variable

sourceChanges
  :: MonadGitHub m
  => RepositoryFullName
  -> ConduitT (These (Maybe RepositoryYaml) (Maybe RepositoryYaml)) Change m ()
sourceChanges name =
  catTheseMaybesC
    .| filterTheseC (/=)
    .| sequenceSinks_
      [ bothTheseC (.repository)
          .| filterTheseC (/=)
          .| mapC (ChangeRepository . Attribute name)
      , bothTheseC (.branch_protection)
          .| catTheseMaybesC
          .| filterTheseC (/=)
          .| mapC (ChangeBranchProtection . Attribute name)
      , bothTheseC (.rulesets.unwrap)
          .| pairTheseOnC (.name)
          .| filterTheseC (/=)
          .| mapC (ChangeRuleset . Attribute name)
      , bothTheseC (.variables.unwrap)
          .| pairTheseOnC (.name)
          .| filterTheseC (/=)
          .| mapC (ChangeVariable . Attribute name)
      ]
