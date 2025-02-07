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
run options =
  case options.command of
    Plan failOnDiff failOnDiffExitCode -> do
      diff <- forChanges options.path prettyPrintChange
      when (diff && failOnDiff) $ exitWith $ ExitFailure failOnDiffExitCode
    Apply -> forChanges_ options.path $ \c -> do
      prettyPrintChange c
      applyChange c `catch` \err -> do
        logGitHubClientError err
        exitFailure
    Import fullNames -> do
      current <- getCurrentRepositoriesYaml $ toList fullNames
      appendPathArgBytes options.path $ renderRepositoriesYaml current

forChanges_
  :: (MonadGitHub m, MonadIO m, MonadLogger m)
  => PathArg
  -> (Change -> m ())
  -> m ()
forChanges_ pathArg f = void $ forChanges pathArg f

-- | Run an action on each 'Change'
--
-- Returns 'False' if there were no changes, 'True' otherwise.
forChanges
  :: (MonadGitHub m, MonadIO m, MonadLogger m)
  => PathArg
  -> (Change -> m a)
  -> m Bool
forChanges pathArg f = do
  mChanges <- nonEmpty <$> buildChanges pathArg
  maybe
    (False <$ logInfo "All repositories up to date")
    ((True <$) . traverse_ f)
    mChanges

buildChanges :: (MonadGitHub m, MonadIO m) => PathArg -> m [Change]
buildChanges pathArg = do
  desired <- getDesiredRepositoriesYaml =<< getPathArgBytes pathArg
  current <- getCurrentRepositoriesYaml $ map (.repository.full_name) desired
  pure $ planChanges desired current
