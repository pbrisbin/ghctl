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
import Data.Yaml qualified as Yaml
import GHCTL.BranchProtection
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.KeyedList
import GHCTL.PathArg
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.Ruleset
import GHCTL.Variable
import Lens.Micro (to, (^?))
import Lens.Micro.Aeson (key, _Array)

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
  :: (MonadIO m, MonadLogger m, MonadMask m) => PathArg -> m [RepositoryYaml]
getDesiredRepositoriesYaml pathArg = do
  withThreadContext ["path" L..= showPathArg pathArg] $ do
    bytes <- getPathArgBytes pathArg
    value <- decodeYamlOrDie @_ @Value bytes
    (mDefaults, repositories) <- decodeTopLevel value
    traverse (fromJSONOrDie . maybe id overwriteValues mDefaults) repositories

decodeTopLevel
  :: (MonadIO m, MonadLogger m) => Value -> m (Maybe Value, [Value])
decodeTopLevel v = do
  maybe
    (invalidConfig "repositories key not present or not array")
    (pure . (mDefaults,))
    mRepositories
 where
  mDefaults = v ^? key "defaults"
  mRepositories = v ^? key "repositories" . _Array . to toList

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
  :: forall m a
   . (HasCodec a, MonadIO m, MonadLogger m)
  => ByteString
  -> m a
decodeYamlOrDie bytes =
  case eitherDecodeYamlViaCodec bytes of
    Left ex -> invalidConfig $ pack (Yaml.prettyPrintParseException ex) :# []
    Right a -> pure a

fromJSONOrDie
  :: forall m a
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Value
  -> m a
fromJSONOrDie value =
  case Aeson.parseEither parseJSONViaCodec value of
    Left err -> invalidConfig $ pack err :# ["input" L..= value]
    Right a -> pure a

overwriteValues :: Value -> Value -> Value
overwriteValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith overwriteValues a b
  (_, v) -> v

invalidConfig :: (MonadIO m, MonadLogger m) => Message -> m a
invalidConfig (msg :# attrs) = do
  logError $ ("Invalid repositories file:\n" <> msg) :# attrs
  exitFailure
