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
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset

data RepositoriesYaml = RepositoriesYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: [Ruleset]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

renderRepositoriesYaml :: [RepositoriesYaml] -> ByteString
renderRepositoriesYaml = mconcat . map (("\n\n---\n" <>) . Yaml.encode)

getDesiredRepositoriesYaml :: MonadIO m => ByteString -> m [RepositoriesYaml]
getDesiredRepositoriesYaml = liftIO . Yaml.decodeAllThrow

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

    pure RepositoriesYaml {repository, branch_protection, rulesets}
