-- |
--
-- Module      : GHCTL.Change.Pretty
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change.Pretty
  ( prettyPrintChange
  ) where

import GHCTL.Prelude

import Blammo.Logging.Colors (Colors (..), getColorsLogger)
import Blammo.Logging.Logger (HasLogger, pushLoggerLn)
import Data.Algorithm.Diff (Diff)
import Data.Algorithm.Diff qualified as Diff
import GHCTL.Change
import GHCTL.Diff
import GHCTL.Repository
import GHCTL.Ruleset
import GHCTL.Variable

prettyPrintChange
  :: (HasLogger env, MonadIO m, MonadReader env m) => Change -> m ()
prettyPrintChange change = do
  colors <- getColorsLogger
  pushLoggerLn $ case change of
    ChangeRepository attr ->
      renderAttribute colors "REPOSITORY" (const Nothing) attr
    ChangeBranchProtection attr ->
      renderAttribute
        colors
        "BRANCH_PROTECTION"
        (const $ Just $ attr.repository.default_branch)
        attr
    ChangeRuleset attr ->
      renderAttribute colors "RULESET" (Just . (.name)) attr
    ChangeVariable attr ->
      renderAttribute colors "VARIABLE" (Just . (.name)) attr

renderAttribute
  :: ToJSON a
  => Colors
  -> Text
  -> (a -> Maybe Text)
  -> Attribute a
  -> Text
renderAttribute colors@Colors {..} target getName attr =
  title <> maybe "" (unlines . ("" :)) diff
 where
  title :: Text
  title =
    unwords
      [ action
      , bold target
      , magenta (toText attr.repository.full_name)
      , maybe "" (\n -> dim "name=" <> cyan n) name
      ]

  action :: Text
  action = case attr.desiredCurrent of
    This _ -> green "CREATE"
    That _ -> red "DELETE"
    These _ _ -> yellow "UPDATE"

  name :: Maybe Text
  name = case attr.desiredCurrent of
    This a -> getName a
    That b -> getName b
    These _ b -> getName b

  diff :: Maybe [Text]
  diff = case attr.desiredCurrent of
    This _ -> Nothing
    That _ -> Nothing
    These a b -> Just $ renderDiffLines colors $ jsonDiff a b

renderDiffLines :: Colors -> [Diff Text] -> [Text]
renderDiffLines colors@Colors {..} diff =
  [ red "--- current"
  , green "+++ desired"
  ]
    <> map (colorizeDiffLine colors) diff

colorizeDiffLine :: Colors -> Diff Text -> Text
colorizeDiffLine Colors {..} = \case
  Diff.First t -> red $ "-" <> t
  Diff.Second t -> green $ "+" <> t
  Diff.Both t _ -> dim $ " " <> t
