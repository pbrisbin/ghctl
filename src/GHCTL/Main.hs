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
import Conduit
import GHCTL.App
import GHCTL.Change
import GHCTL.Change.Apply
import GHCTL.Change.Pretty
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub.Client.Error (logGitHubClientError)
import GHCTL.Options
import GHCTL.RepositoriesYaml
import GHCTL.Repository
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
     , MonadReader env m
     , MonadUnliftIO m
     )
  => Options
  -> m ()
run options = do
  logInfo $ "Loading desired state" :# ["directory" .= options.dir]
  desired <- getDesiredRepositoriesYaml options.dir

  let
    names = map (.repository.full_name) desired
    count = length names

  logInfo $ "Loading remote state" :# ["repositories" .= show @Text count]
  current <- getCurrentRepositoriesYaml names

  case options.command of
    Plan failOnDiff failOnDiffExitCode -> do
      logInfo "Sourcing differences"
      diff <-
        runConduit
          $ sourceChanges current desired
          .| iterMC prettyPrintChange
          .| lengthC @_ @Int

      logInfo $ ("Repository differences: " <> show diff) :# []
      when (diff > 0 && failOnDiff) $ exitWith $ ExitFailure failOnDiffExitCode
    Apply -> do
      logInfo "Sourcing and applying differences"
      runConduit
        $ sourceChanges current desired
        .| iterMC prettyPrintChange
        .| iterMC applyChange
        .| sinkNull
