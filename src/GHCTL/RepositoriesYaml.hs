-- |
--
-- Module      : GHCTL.RepositoriesYaml
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoriesYaml
  ( RepositoriesYaml (..)
  , getDesiredRepositoriesYaml
  , getCurrentRepositoriesYaml
  , renderRepositoriesYaml
  ) where

import GHCTL.Prelude

import Data.Yaml qualified as Yaml
import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.PathArg
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable

data RepositoriesYaml = RepositoriesYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: [Ruleset]
  , variables :: [Variable]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

renderRepositoriesYaml :: [RepositoriesYaml] -> ByteString
renderRepositoriesYaml = mconcat . map (("\n\n---\n" <>) . Yaml.encode)

getDesiredRepositoriesYaml
  :: (MonadIO m, MonadLogger m) => PathArg -> m [RepositoriesYaml]
getDesiredRepositoriesYaml pathArg = do
  bytes <- getPathArgBytes pathArg

  case Yaml.decodeAllThrow bytes of
    Left ex -> do
      let
        message :: Text
        message =
          ("Exception decoding repositories file:\n" <>)
            $ pack
            $ maybe (displayException ex) Yaml.prettyPrintParseException
            $ fromException ex

      logError $ message :# ["path" .= showPathArg pathArg]
      exitFailure
    Right yamls -> pure yamls

getCurrentRepositoriesYaml
  :: MonadGitHub m => [RepositoryFullName] -> m [RepositoriesYaml]
getCurrentRepositoriesYaml = foldMapM getCurrent

getCurrent :: MonadGitHub m => RepositoryFullName -> m [RepositoriesYaml]
getCurrent name = do
  mRepo <- GitHub.getRepository name.owner name.name

  fmap maybeToList $ for mRepo $ \repository -> do
    branch_protection <-
      GitHub.getBranchProtection
        name.owner
        name.name
        repository.default_branch

    rulesets <- do
      rs <- GitHub.getAllRepositoryRulesets name.owner name.name
      traverse (GitHub.getRepositoryRuleset name.owner name.name . (.id)) rs

    Variables {variables} <- GitHub.listRepositoryVariables name.owner name.name

    pure RepositoriesYaml {repository, branch_protection, rulesets, variables}
