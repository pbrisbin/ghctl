module GHCTL.Main
  ( main
  ) where

import GHCTL.Prelude

import GHCTL.App
import GHCTL.Change
import GHCTL.Change.Apply
import GHCTL.Change.Pretty
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub.Client.Error (logGitHubClientError)
import GHCTL.Options
import GHCTL.RepositoriesYaml
import GHCTL.Repository

main :: IO ()
main = do
  options <- parseOptions

  runAppM $ case options.command of
    Plan -> forChanges_ options.path prettyPrintChange
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
forChanges_ pathArg f = do
  mChanges <- nonEmpty <$> buildChanges pathArg
  maybe (logInfo "All repositories up to date") (traverse_ f) mChanges

buildChanges :: (MonadGitHub m, MonadIO m) => PathArg -> m [Change]
buildChanges pathArg = do
  desired <- getDesiredRepositoriesYaml =<< getPathArgBytes pathArg
  current <- getCurrentRepositoriesYaml $ map (.repository.full_name) desired
  pure $ planChanges desired current
