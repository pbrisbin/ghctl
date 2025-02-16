-- |
--
-- Module      : GHCTL.GitHub.Client
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.GitHub.Client
  ( getGitHub
  , getGitHubMaybe
  , postGitHub
  , putGitHub

    -- * Lower-level
  , GitHubRequest (..)
  , githubRequest
  ) where

import GHCTL.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import GHCTL.GitHub.Client.Error
import GHCTL.GitHub.Token
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple
  ( addRequestHeader
  , getResponseBody
  , getResponseStatus
  , httpLBS
  , parseRequest
  , setRequestBodyJSON
  , setRequestMethod
  )
import Network.HTTP.Types.Header
  ( hAccept
  , hAuthorization
  , hContentType
  , hUserAgent
  )
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)

data GitHubRequest body a = GitHubRequest
  { path :: Text
  , method :: ByteString
  , body :: Maybe body
  , onSuccess :: BSL.ByteString -> Either String a
  , onNotFound :: Maybe a
  }

-- | Use this in place of 'Nothing' for 'body'-less 'GitHubRequest's, so that
-- 'ToJSON' can be solved
noBody :: Maybe ()
noBody = Nothing

getGitHub
  :: ( FromJSON a
     , HasGitHubToken env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     )
  => Text -> m a
getGitHub path =
  githubRequest
    $ GitHubRequest
      { path
      , method = "GET"
      , body = noBody
      , onSuccess = Aeson.eitherDecode
      , onNotFound = Nothing
      }

getGitHubMaybe
  :: ( FromJSON a
     , HasGitHubToken env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     )
  => Text -> m (Maybe a)
getGitHubMaybe path =
  githubRequest
    $ GitHubRequest
      { path
      , method = "GET"
      , body = noBody
      , onSuccess = fmap Just . Aeson.eitherDecode
      , onNotFound = Just Nothing
      }

postGitHub
  :: ( HasGitHubToken env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     , ToJSON body
     )
  => Text -> body -> m ()
postGitHub path body =
  githubRequest
    $ GitHubRequest
      { path
      , method = "POST"
      , body = Just body
      , onSuccess = const $ Right ()
      , onNotFound = Nothing
      }

putGitHub
  :: ( HasGitHubToken env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     , ToJSON body
     )
  => Text -> body -> m ()
putGitHub path body =
  githubRequest
    $ GitHubRequest
      { path
      , method = "PUT"
      , body = Just body
      , onSuccess = const $ Right ()
      , onNotFound = Nothing
      }

githubRequest
  :: ( HasGitHubToken env
     , MonadIO m
     , MonadLogger m
     , MonadReader env m
     , ToJSON body
     )
  => GitHubRequest body a -> m a
githubRequest GitHubRequest {path, method, body, onSuccess, onNotFound} = do
  token <- asks getGitHubToken
  req <-
    liftIO
      $ addRequestHeader hUserAgent "ghctl/vTODO"
      . addRequestHeader hAuthorization ("token " <> token.unwrap)
      . addRequestHeader hAccept "application/json"
      . addRequestHeader hContentType "application/json"
      . setRequestMethod method
      . maybe id setRequestBodyJSON body
      <$> parseRequest ("https://api.github.com" <> unpack path)

  logDebug $ decodeUtf8 (HTTP.method req <> " " <> HTTP.path req) :# []

  resp <- httpLBS req

  let
    status = getResponseStatus resp
    reqBody = getResponseBody resp

  if statusIsSuccessful status
    then either (throwIO . JSONDecodeError reqBody) pure $ onSuccess reqBody
    else case statusCode status of
      404 | Just x <- onNotFound -> pure x
      _ -> throwIO $ ResponseError resp
