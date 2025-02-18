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
import Data.These.Combinators (bimapThese)
import GHCTL.BranchProtection
import GHCTL.Conduit
import GHCTL.Diff
import GHCTL.GitHub (MonadGitHub)
import GHCTL.KeyedList
import GHCTL.RepositoriesYaml
import GHCTL.Repository
import GHCTL.Ruleset
import GHCTL.Variable

data Change
  = ChangeRepository (Attribute Repository)
  | ChangeBranchProtection (Attribute BranchProtection)
  | ChangeRuleset (Attribute Ruleset)
  | ChangeVariable (Attribute Variable)

data Attribute a = Attribute
  { repository :: Repository
  , desiredCurrent :: These a a
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON) -- logging

sourceChanges
  :: (HasLogger env, MonadGitHub m, MonadLogger m, MonadReader env m)
  => ConduitT RepositoryYaml Change m ()
sourceChanges = awaitForever $ \desired -> do
  let name = desired.repository.full_name
  logDebug $ "Loading remote state" :# ["repository" .= name]
  current <- lift $ getCurrentRepositoryYaml name

  colors <- getColorsLogger

  yield (These (Just desired) current)
    .| catTheseMaybesC
    .| filterTheseC (/=)
    .| with
      getRepository
      ( \repository ->
          sequenceSinks_
            [ bothTheseC (.repository)
                .| filterTheseC (/=)
                .| iterMC (logChange colors repository Nothing)
                .| mapC (ChangeRepository . Attribute repository)
            , bothTheseC (.branch_protection)
                .| catTheseMaybesC
                .| filterTheseC (/=)
                .| iterMC (logChange colors repository $ Just "branch-protection")
                .| mapC (ChangeBranchProtection . Attribute repository)
            , bothTheseC (.rulesets.unwrap)
                .| pairTheseOnC (.name)
                .| filterTheseC (/=)
                .| iterMC (logChange colors repository $ Just "ruleset")
                .| mapC (ChangeRuleset . Attribute repository)
            , bothTheseC (.variables.unwrap)
                .| pairTheseOnC (.name)
                .| filterTheseC (/=)
                .| iterMC (logChange colors repository $ Just "variable")
                .| mapC (ChangeVariable . Attribute repository)
            ]
      )

getRepository :: These RepositoryYaml RepositoryYaml -> Repository
getRepository = mergeThese const . bimapThese (.repository) (.repository)

with :: Monad m => (i -> a) -> (a -> ConduitT i o m ()) -> ConduitT i o m ()
with f g = awaitForever $ \i -> yield i .| g (f i)

logChange
  :: forall m a
   . (MonadLogger m, ToJSON a)
  => Colors
  -> Repository
  -> Maybe Text
  -> These a a
  -> m ()
logChange colors@Colors {..} repository mTarget = \case
  This desired -> logInfo $ "Create " <> suffix :# ["create" .= desired]
  That current -> logInfo $ "Delete " <> suffix :# ["delete" .= current]
  These desired current -> do
    logInfo $ "Update " <> suffix :# []
    traverse_ (logOther (LevelOther "") . (:# []))
      $ renderDiffLines colors
      $ jsonDiff current desired
 where
  suffix = magenta (toText repository.full_name) <> maybe "" (dim . ("#" <>)) mTarget

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
