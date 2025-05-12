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
import Data.Text.IO qualified as T
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
import GHCTL.SchemaGen
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

  colors <- getColorsLogger

  case options.command of
    Plan poptions -> do
      changes <- withChanges dir poptions.repositories $ logChange colors

      if null changes
        then logInfo "No differences found"
        else do
          logError $ "Differences found" :# ["count" .= length changes]
          when (poptions.failOnDiff)
            $ exitWith
            $ ExitFailure poptions.failOnDiffExitCode
    Apply aoptions -> do
      void $ withChanges dir aoptions.repositories $ \change -> do
        logChange colors change
        applyChange aoptions.delete change
    Schema -> liftIO $ T.putStrLn prettySchema

withChanges
  :: (MonadGitHub m, MonadIO m, MonadLogger m, MonadMask m)
  => Path Abs Dir
  -> Maybe (NonEmpty RepositoryFullName)
  -> (Change -> m ())
  -> m [Change]
withChanges dir repositories f = do
  desired <- withThreadContext ["dir" .= toFilePath dir] $ do
    logDebug $ "Loading desired state" :# ["dir" .= toFilePath dir]
    getDesiredRepositoryYamls dir repositories

  runConduit
    $ yieldMany (Map.toList desired)
    .| awaitForever (uncurry sourceChanges')
    .| iterMC f
    .| sinkList

sourceChanges'
  :: MonadGitHub m
  => RepositoryFullName
  -> Maybe RepositoryYaml
  -> ConduitT i Change m ()
sourceChanges' name desired = do
  current <- lift $ getCurrentRepositoryYaml name
  yield (These desired current) .| sourceChanges name
