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

import Blammo.Logging.Logger (HasLogger)
import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Conduit
import Data.HashMap.Strict qualified as HashMap
import GHCTL.App
import GHCTL.Change
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub.Client.Error (logGitHubClientError)
import GHCTL.Options
import GHCTL.RepositoriesYaml
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
    getDesiredRepositoriesYaml dir options.repositories

  changes <-
    runConduit
      $ yieldMany (HashMap.toList desired)
      .| awaitForever
        ( \(name, x) -> do
            logDebug $ "Loading remote state" :# ["repository" .= name]
            current <- lift $ getCurrentRepositoryYaml name
            logDebug $ "Sourcing changes" :# ["repository" .= name]
            yield (These x current) .| sourceChanges name
        )
      .| sinkList

  logInfo
    $ "Repositories sync complete"
    :# [ "changesApplied" .= (if options.apply then length changes else 0)
       , "changesFound" .= length changes
       , "unchanged" .= (length desired - length changes)
       ]

  unless (null changes || options.apply || not options.failOnDiff) $ do
    logError "Failing due to differences found (--fail-on-diff)"
    exitWith $ ExitFailure options.failOnDiffExitCode
