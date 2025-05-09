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

import Blammo.Logging.Colors (Colors (..), getColorsLogger)
import Blammo.Logging.Logger (HasLogger)
import Data.Algorithm.Diff (Diff)
import Data.Algorithm.Diff qualified as Diff
import GHCTL.BranchProtection
import GHCTL.Conduit
import GHCTL.Diff
import GHCTL.GitHub (MonadGitHub)
import GHCTL.KeyedList
import GHCTL.RepositoriesYaml
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

sourceChanges
  :: (HasLogger env, MonadGitHub m, MonadLogger m, MonadReader env m)
  => RepositoryFullName
  -> ConduitT (These (Maybe RepositoryYaml) (Maybe RepositoryYaml)) Change m ()
sourceChanges name = do
  colors <- getColorsLogger

  catTheseMaybesC
    .| filterTheseC (/=)
    .| sequenceSinks_
      [ bothTheseC (.repository)
          .| filterTheseC (/=)
          .| iterMC (logChange colors name Nothing)
          .| mapC (ChangeRepository . Attribute name)
      , bothTheseC (.branch_protection)
          .| catTheseMaybesC
          .| filterTheseC (/=)
          .| iterMC (logChange colors name $ Just "branch-protection")
          .| mapC (ChangeBranchProtection . Attribute name)
      , bothTheseC (.rulesets.unwrap)
          .| pairTheseOnC (.name)
          .| filterTheseC (/=)
          .| iterMC (logChange colors name $ Just "ruleset")
          .| mapC (ChangeRuleset . Attribute name)
      , bothTheseC (.variables.unwrap)
          .| pairTheseOnC (.name)
          .| filterTheseC (/=)
          .| iterMC (logChange colors name $ Just "variable")
          .| mapC (ChangeVariable . Attribute name)
      ]

logChange
  :: forall m a
   . (MonadLogger m, ToJSON a)
  => Colors
  -> RepositoryFullName
  -> Maybe Text
  -> These a a
  -> m ()
logChange colors@Colors {..} name mTarget = \case
  This desired -> logInfo $ "Create " <> suffix :# ["create" .= desired]
  That current -> logInfo $ "Delete " <> suffix :# ["delete" .= current]
  These desired current -> do
    logInfo $ "Update " <> suffix :# []
    traverse_ (logOther (LevelOther "") . (:# []))
      $ renderDiffLines colors
      $ jsonDiff current desired
 where
  suffix = magenta (toText name) <> maybe "" (dim . ("#" <>)) mTarget

renderDiffLines :: Colors -> [Diff Text] -> [Text]
renderDiffLines colors@Colors {..} diff =
  red "--- current"
    : green "+++ desired"
    : map (colorizeDiffLine colors) diff

colorizeDiffLine :: Colors -> Diff Text -> Text
colorizeDiffLine Colors {..} = \case
  Diff.First t -> red $ "-" <> t
  Diff.Second t -> green $ "+" <> t
  Diff.Both t _ -> dim $ " " <> t
