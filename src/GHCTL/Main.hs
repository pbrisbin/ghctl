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
import Blammo.Logging.ThreadContext (MonadMask)
import Conduit
import GHCTL.App
import GHCTL.Change
import GHCTL.Change.Apply
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub.Client.Error (logGitHubClientError)
import GHCTL.Options
import GHCTL.PathArg
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
     , MonadMask m
     , MonadReader env m
     , MonadUnliftIO m
     )
  => Options
  -> m ()
run options = do
  logDebug $ "Loading desired state" :# ["path" .= showPathArg options.path]
  desired <- getDesiredRepositoriesYaml options.path
  changes <-
    runConduit
      $ yieldMany desired
      .| filterC include
      .| sourceChanges
      .| iterMC apply
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
 where
  include =
    maybe
      (const True)
      (\names -> (`elem` names) . (.repository.full_name))
      options.repositories

  apply change =
    when options.apply $ do
      logInfo "Applying change"
      applyChange change
