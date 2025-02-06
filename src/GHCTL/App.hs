-- |
--
-- Module      : GHCTL.App
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.App
  ( AppM (..)
  , runAppM
  ) where

import GHCTL.Prelude

import Blammo.Logging.Simple
import Control.Lens
import GHCTL.GitHub (MonadGitHub (..))
import GHCTL.GitHub.Actual
import GHCTL.GitHub.Token

data App = App
  { logger :: Logger
  , githubToken :: GitHubToken
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  getGitHubToken app = app.githubToken

newtype AppM a = AppM
  { unwrap :: ReaderT App IO a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader App
    , MonadUnliftIO
    )
  deriving (MonadLogger) via WithLogger App IO
  deriving (MonadLoggerIO) via WithLogger App IO
  deriving (MonadGitHub) via ActualGitHub App IO

runAppM :: AppM a -> IO a
runAppM f = do
  withLoggerEnv $ \logger -> do
    app <- App logger <$> envGitHubToken
    runReaderT f.unwrap app
