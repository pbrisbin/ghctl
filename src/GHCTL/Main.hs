-- |
--
-- Module      : GHCTL.Main
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Main
  ( main
  ) where

import GHCTL.Prelude

import Blammo.Logging.Colors (getColorsLogger)
import Blammo.Logging.Logger (HasLogger)
import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Conduit
import Data.Map.Strict qualified as Map
import GHCTL.App
import GHCTL.Change
import GHCTL.Change.Apply
import GHCTL.Change.Log
import GHCTL.Change.Source
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub.Client.Error (logGitHubClientError)
import GHCTL.Options
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import GHCTL.RepositoryYaml.Fetch
import GHCTL.RepositoryYaml.Load
import Path (SomeBase (..))
import Path.IO (getCurrentDir)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  options <- parseOptions

  runAppM $ do
    run options `catch` \err -> do
      logGitHubClientError err
      exitFailure

run
  :: ( HasLogger env
     , MonadGitHub m
     , MonadLogger m
     , MonadMask m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => Options
  -> m ()
run options = do
  dir <- case options.dir of
    Abs dir -> pure dir
    Rel dir -> (</> dir) <$> getCurrentDir

  desired <- withThreadContext ["dir" .= toFilePath dir] $ do
    logDebug $ "Loading desired state" :# ["dir" .= toFilePath dir]
    getDesiredRepositoryYamls dir options.repositories

  colors <- getColorsLogger

  logDebug
    $ "Sourcing changes"
    :# ["numRepos" .= length desired, "willApply" .= options.apply]

  changes <-
    runConduit
      $ yieldMany (Map.toList desired)
      .| sourceRepositoryChanges
      .| iterMC (logChange colors)
      .| iterMC (when options.apply . applyChange options.delete)
      .| sinkList

  logInfo
    $ "Repositories sync complete"
    :# [ "changesApplied" .= (if options.apply then length changes else 0)
       , "changesFound" .= length changes
       ]

  unless (null changes || options.apply || not options.failOnDiff) $ do
    logError "Failing due to differences found (--fail-on-diff)"
    exitWith $ ExitFailure options.failOnDiffExitCode

sourceRepositoryChanges
  :: MonadGitHub m
  => ConduitT (RepositoryFullName, Maybe RepositoryYaml) Change m ()
sourceRepositoryChanges = awaitForever $ \(name, desired) -> do
  current <- lift $ getCurrentRepositoryYaml name
  yield (These desired current) .| sourceChanges name
