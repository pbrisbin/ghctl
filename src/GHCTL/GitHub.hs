module GHCTL.GitHub
  ( Identified (..)
  , MonadGitHub (..)
  ) where

import GHCTL.Prelude

import GHCTL.BranchProtection
import GHCTL.Identified
import GHCTL.Repository
import GHCTL.Ruleset

class Monad m => MonadGitHub m where
  getRepository :: Text -> Text -> m (Maybe Repository)

  getBranchProtection :: Text -> Text -> Text -> m (Maybe BranchProtection)

  getAllRepositoryRulesets :: Text -> Text -> m [Identified]

  getRepositoryRuleset :: Text -> Text -> Int -> m Ruleset

  getRepositoryRulesetIdByName :: Text -> Text -> Text -> m (Maybe Int)
  getRepositoryRulesetIdByName owner repo name = do
    rulesets <- getAllRepositoryRulesets owner repo
    pure $ (.id) <$> find ((== name) . (.name)) rulesets

  updateRepository :: Text -> Text -> Repository -> m ()

  updateRepositoryRuleset :: Text -> Text -> Int -> Ruleset -> m ()
