-- |
--
-- Module      : GHCTL.RepositoryYaml.Load
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoryYaml.Load
  ( getDesiredRepositoryYamls
  ) where

import GHCTL.Prelude

import Autodocodec (HasCodec, parseJSONViaCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Yaml qualified as Yaml
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import Path (reldir, relfile, splitExtension)
import Path.IO (doesFileExist, listDirRecurRel)

-- | Load on-disk state as a map of 'RepositoryYaml's by name
getDesiredRepositoryYamls
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Path Abs Dir
  -> Maybe (NonEmpty RepositoryFullName)
  -> m (Map RepositoryFullName (Maybe RepositoryYaml))
getDesiredRepositoryYamls dir mNames = do
  let
    defaultsFile = dir </> [relfile|defaults.yaml|]
    repositoriesDir = dir </> [reldir|repositories|]

  defaultsExist <- doesFileExist defaultsFile
  defaults <-
    if defaultsExist
      then decodeYamlOrDie defaultsFile
      else pure $ Object mempty

  (_, yamls) <- listDirRecurRel repositoriesDir
  repositories <- foldMapM (loadRepository defaults repositoriesDir) yamls

  pure $ maybe repositories (`filterAndPadNothings` repositories) mNames

filterAndPadNothings
  :: Ord k
  => NonEmpty k
  -> Map k (Maybe a)
  -> Map k (Maybe a)
filterAndPadNothings =
  Map.merge onGiven onPresent onBoth . foldMap (`Map.singleton` Nothing)
 where
  -- given, not present -> keep a Nothing value
  onGiven = Map.preserveMissing

  -- present, not given -> filter out
  onPresent = Map.dropMissing

  -- given and present -> keep
  onBoth = Map.zipWithMatched $ const $ const id

loadRepository
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Value
  -- ^ Defaults
  -> Path b Dir
  -- ^ Parent that path is relative to
  -> Path Rel File
  -- ^ Repository path, expected to be @{owner}/{name}.yaml@
  -> m (Map RepositoryFullName (Maybe RepositoryYaml))
loadRepository defaults dir path = do
  withThreadContext ["path" .= toFilePath path] $ do
    case parseRepositoryPath path of
      Left err -> do
        logWarn $ "Repository path is invalid, skipping" :# ["error" .= err]
        pure mempty
      Right name -> do
        val <- decodeYamlOrDie $ dir </> path
        yaml <- fromJSONOrDie $ overwriteValues defaults val
        pure $ Map.singleton name $ Just yaml

parseRepositoryPath :: Path Rel File -> Either String RepositoryFullName
parseRepositoryPath path =
  case splitExtension path of
    Nothing -> Left "path must have .yaml extension (saw none)"
    Just (base, ".yaml") -> repositoryFullNameFromText $ pack $ toFilePath base
    Just (_, ext) -> Left $ "path must have .yaml extension (saw " <> ext <> ")"

overwriteValues :: Value -> Value -> Value
overwriteValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith overwriteValues a b
  (_, v) -> v

decodeYamlOrDie
  :: forall m a b
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Path b File
  -> m a
decodeYamlOrDie path = do
  bytes <- readFileBS $ toFilePath path
  fromRightOrDie err $ eitherDecodeYamlViaCodec bytes
 where
  err ex = pack (Yaml.prettyPrintParseException ex) :# []

fromJSONOrDie
  :: forall m a
   . (HasCodec a, MonadIO m, MonadLogger m)
  => Value
  -> m a
fromJSONOrDie value =
  fromRightOrDie err $ Aeson.parseEither parseJSONViaCodec value
 where
  err ex = pack ex :# ["input" .= value]
