module GHCTL.Config
  ( Config (..)
  , loadConfig
  ) where

import GHCTL.Prelude

import Blammo.Logging.ThreadContext (MonadMask)
import Data.Aeson (Value (..))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import GHCTL.RepositoryYaml.Load
import Path (reldir, relfile)
import Path.IO (doesFileExist, listDirRecurRel)

data Config = Config
  { defaultsFile :: Maybe (Path Abs File)
  , defaults :: Value
  , repositoriesDir :: Path Abs Dir
  , repositories :: Map RepositoryFullName (Maybe RepositoryYaml)
  }

loadConfig
  :: (MonadIO m, MonadLogger m, MonadMask m)
  => Path Abs Dir
  -- ^ @.ghctl@
  -> Maybe (NonEmpty RepositoryFullName)
  -- ^ Optional list of repositories to filter to
  -> m Config
loadConfig dir mNames = do
  let
    defaultsFiles =
      [ dir </> [relfile|defaults.yml|]
      , dir </> [relfile|defaults.yaml|]
      ]
    repositoriesDir = dir </> [reldir|repositories|]

  defaultsFile <- getFileExists defaultsFiles
  defaults <- maybe (pure $ Object mempty) decodeYamlOrDie defaultsFile

  (_, yamls) <- listDirRecurRel repositoriesDir
  repositories <- foldMapM (loadRepositoryYaml defaults repositoriesDir) yamls
  pure
    $ Config
      { defaultsFile
      , defaults
      , repositoriesDir
      , repositories = maybe repositories (`filterAndPadNothings` repositories) mNames
      }

getFileExists :: MonadIO m => [Path b File] -> m (Maybe (Path b File))
getFileExists = \case
  [] -> pure Nothing
  (p : ps) -> do
    exists <- doesFileExist p
    if exists
      then pure $ Just p
      else getFileExists ps

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
