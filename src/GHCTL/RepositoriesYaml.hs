-- |
--
-- Module      : GHCTL.RepositoriesYaml
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoriesYaml
  ( RepositoryYaml (..)
  , getDesiredRepositoriesYaml
  , getCurrentRepositoryYaml
  ) where

import GHCTL.Prelude hiding (die, (.=))

import Autodocodec
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Blammo.Logging qualified as L
import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Foldable1 (foldMap1)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.KeyedList
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable
import Path (reldir, relfile, splitExtension)
import Path.IO (doesFileExist, listDirRecurRel)

data RepositoryYaml = RepositoryYaml
  { repository :: Repository
  , branch_protection :: Maybe BranchProtection
  , rulesets :: KeyedList "name" Ruleset
  , variables :: KeyedList "name" Variable
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RepositoryYaml)

instance HasCodec RepositoryYaml where
  codec =
    object "RepositoryYaml"
      $ RepositoryYaml
      <$> (requiredField' "repository" .= (.repository))
      <*> (optionalFieldOrNull' "branch_protection" .= (.branch_protection))
      <*> (requiredField' "rulesets" .= (.rulesets))
      <*> (requiredField' "variables" .= (.variables))

getDesiredRepositoriesYaml
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Path Abs Dir
  -> Maybe (NonEmpty RepositoryFullName)
  -> m (HashMap RepositoryFullName (Maybe RepositoryYaml))
getDesiredRepositoriesYaml dir mNames = do
  let
    defaultsPath = dir </> [relfile|defaults.yaml|]
    repositoriesPath = dir </> [reldir|repositories|]

  defaultsExist <- doesFileExist defaultsPath
  defaults <-
    if defaultsExist
      then decodeYamlOrDie defaultsPath
      else pure $ Object mempty

  (_, repositoryYamls) <- listDirRecurRel repositoriesPath
  repositories <-
    foldMapM (loadRepository defaults repositoriesPath) repositoryYamls

  pure
    $ maybe
      repositories
      ( \names ->
          HashMap.intersection
            (foldMap1 (`HashMap.singleton` Nothing) names)
            repositories
      )
      mNames

loadRepository
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Value
  -- ^ Defaults
  -> Path b Dir
  -- ^ Parent that path is relative to
  -> Path Rel File
  -- ^ Repository path, expected to be @{owner}/{name}.yaml@
  -> m (HashMap RepositoryFullName (Maybe RepositoryYaml))
loadRepository defaults dir path = do
  withThreadContext ["path" L..= toFilePath path] $ do
    case parseRepositoryPath path of
      Left err -> logErrorDie $ "Invalid repositories path:\n" <> pack err :# []
      Right name -> do
        val <- decodeYamlOrDie $ dir </> path
        yaml <- fromJSONOrDie $ overwriteValues defaults val
        pure $ HashMap.singleton name $ Just yaml

parseRepositoryPath :: Path Rel File -> Either String RepositoryFullName
parseRepositoryPath path =
  case splitExtension path of
    Nothing -> Left "path must have yaml extension (saw none)"
    Just (base, ".yaml") -> repositoryFullNameFromText $ pack $ toFilePath base
    Just (_, ext) -> Left $ "path must have .yaml extenstion (saw " <> ext <> ")"

getCurrentRepositoryYaml
  :: MonadGitHub m => RepositoryFullName -> m (Maybe RepositoryYaml)
getCurrentRepositoryYaml name = do
  mRepo <- GitHub.getRepository name.owner name.name

  for mRepo $ \repository -> do
    branch_protection <-
      GitHub.getBranchProtection
        name.owner
        name.name
        repository.default_branch

    rulesets <- fmap KeyedList $ do
      rs <- GitHub.getAllRepositoryRulesets name.owner name.name
      traverse (GitHub.getRepositoryRuleset name.owner name.name . (.id)) rs

    variables <-
      KeyedList . (.variables) <$> GitHub.listRepositoryVariables name.owner name.name

    pure RepositoryYaml {repository, branch_protection, rulesets, variables}

decodeYamlOrDie
  :: forall m a b
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Path b File
  -> m a
decodeYamlOrDie path = do
  bytes <- readFileBS $ toFilePath path
  case eitherDecodeYamlViaCodec bytes of
    Left ex -> do
      let msg =
            "Error parsing "
              <> pack (toFilePath path)
              <> "\n"
              <> pack (Yaml.prettyPrintParseException ex)
      logErrorDie $ msg :# []
    Right a -> pure a

fromJSONOrDie
  :: forall m a
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Value
  -> m a
fromJSONOrDie value =
  case Aeson.parseEither parseJSONViaCodec value of
    Left err -> logErrorDie $ pack err :# ["input" L..= value]
    Right a -> pure a

overwriteValues :: Value -> Value -> Value
overwriteValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith overwriteValues a b
  (_, v) -> v

logErrorDie :: (MonadIO m, MonadLogger m) => Message -> m a
logErrorDie (msg :# attrs) = do
  logError $ reindent msg :# attrs
  exitFailure
 where
  reindent = T.intercalate "\n       " . lines
