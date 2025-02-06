{-# LANGUAGE RecordWildCards #-}

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

prettyPrintChange
  :: (HasLogger env, MonadIO m, MonadReader env m) => Change -> m ()
prettyPrintChange change = do
  colors <- getColorsLogger
  pushLoggerLn $ renderChangeDescription colors $ toChangeDescription change

data ChangeDescription = ChangeDescription
  { action :: Action
  , target :: Text
  , repository :: Repository
  , name :: Maybe Text
  , diff :: Maybe [Diff Text]
  }

data Action = Create | Update | Delete

toChangeDescription :: Change -> ChangeDescription
toChangeDescription = \case
  CreateRepository repository ->
    ChangeDescription
      { action = Create
      , target = "REPOSITORY"
      , repository = repository
      , name = Nothing
      , diff = Nothing
      }
  DeleteRepository repository ->
    ChangeDescription
      { action = Delete
      , target = "REPOSITORY"
      , repository = repository
      , name = Nothing
      , diff = Nothing
      }
  UpdateRepository desired current ->
    ChangeDescription
      { action = Update
      , target = "REPOSITORY"
      , repository = desired
      , name = Nothing
      , diff = Just $ jsonDiff current desired
      }
  CreateBranchProtection repository _ ->
    ChangeDescription
      { action = Create
      , target = "BRANCH_PROTECTION"
      , repository = repository
      , name = Just repository.default_branch
      , diff = Nothing
      }
  DeleteBranchProtection repository _ ->
    ChangeDescription
      { action = Delete
      , target = "BRANCH_PROTECTION"
      , repository = repository
      , name = Just repository.default_branch
      , diff = Nothing
      }
  UpdateBranchProtection repository desired current ->
    ChangeDescription
      { action = Update
      , target = "BRANCH_PROTECTION"
      , repository = repository
      , name = Just repository.default_branch
      , diff = Just $ jsonDiff current desired
      }
  CreateRuleset repository ruleset ->
    ChangeDescription
      { action = Create
      , target = "RULESET"
      , repository = repository
      , name = Just ruleset.name
      , diff = Nothing
      }
  DeleteRuleset repository ruleset ->
    ChangeDescription
      { action = Delete
      , target = "RULESET"
      , repository = repository
      , name = Just ruleset.name
      , diff = Nothing
      }
  UpdateRuleset repository desired current ->
    ChangeDescription
      { action = Update
      , target = "RULESET"
      , repository = repository
      , name = Just desired.name
      , diff = Just $ jsonDiff current desired
      }

renderChangeDescription :: Colors -> ChangeDescription -> Text
renderChangeDescription Colors {..} ci =
  unlines $ titleLine : maybe [] renderDiffLines ci.diff
 where
  titleLine :: Text
  titleLine =
    unwords
      [ case ci.action of
          Create -> green "CREATE"
          Update -> yellow "UPDATE"
          Delete -> red "DELETE"
      , bold ci.target
      , magenta (toText ci.repository.full_name)
      , maybe "" (\n -> dim "name=" <> cyan n) ci.name
      ]

  renderDiffLines :: [Diff Text] -> [Text]
  renderDiffLines =
    (red "--- current" :)
      . (green "+++ desired" :)
      . map colorizeDiffLine

  colorizeDiffLine = \case
    Diff.First t -> red $ "-" <> t
    Diff.Second t -> green $ "+" <> t
    Diff.Both t _ -> dim $ " " <> t
