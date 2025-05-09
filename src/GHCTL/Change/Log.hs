module GHCTL.Change.Log
  ( logChange
  ) where

import GHCTL.Prelude

import Blammo.Logging.Colors (Colors (..))
import Data.Algorithm.Diff (Diff)
import Data.Algorithm.Diff qualified as Diff
import GHCTL.Change
import GHCTL.Diff
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable

logChange :: MonadLogger m => Colors -> Change -> m ()
logChange colors = \case
  ChangeRepository attr -> logChange' colors attr.repository Nothing attr.desiredCurrent
  ChangeBranchProtection attr -> do
    let suffix = "branch-protection.default-branch"
    logChange' colors attr.repository (Just suffix) attr.desiredCurrent
  ChangeRuleset attr -> do
    let suffix = "ruleset." <> coalesceThese (.name) attr.desiredCurrent
    logChange' colors attr.repository (Just suffix) attr.desiredCurrent
  ChangeVariable attr -> do
    let suffix = "variable." <> coalesceThese (.name) attr.desiredCurrent
    logChange' colors attr.repository (Just suffix) attr.desiredCurrent
 where
  coalesceThese :: (a -> b) -> These a a -> b
  coalesceThese f = mergeTheseWith f f const

logChange'
  :: forall m a
   . (MonadLogger m, ToJSON a)
  => Colors
  -> RepositoryFullName
  -> Maybe Text
  -> These a a
  -> m ()
logChange' colors@Colors {..} name mTarget = \case
  This desired -> logInfo $ green "Create " <> suffix :# ["create" .= desired]
  That _ -> logInfo $ red "Delete " <> suffix :# []
  These desired current -> do
    logInfo $ yellow "Update " <> suffix :# []
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
