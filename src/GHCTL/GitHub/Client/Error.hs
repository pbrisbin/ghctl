-- |
--
-- Module      : GHCTL.GitHub.Client.Error
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.GitHub.Client.Error
  ( GitHubClientError (..)
  , logGitHubClientError
  ) where

import GHCTL.Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Network.HTTP.Simple (Response, getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status (statusCode)

data GitHubClientError
  = JSONDecodeError BSL.ByteString String
  | ResponseError (Response BSL.ByteString)
  deriving stock (Show)
  deriving anyclass (Exception)

logGitHubClientError :: MonadLogger m => GitHubClientError -> m ()
logGitHubClientError =
  logError . \case
    JSONDecodeError body err -> jsonDecodeErrorMessage body err
    ResponseError resp -> responseErrorMessage resp

jsonDecodeErrorMessage :: BSL.ByteString -> String -> Message
jsonDecodeErrorMessage body err =
  "Unable to decode GitHub response"
    :# [ "error" .= err
       , "input" .= decodeUtf8 @Text body
       ]

responseErrorMessage :: Response BSL.ByteString -> Message
responseErrorMessage resp =
  case Aeson.decode body of
    Nothing ->
      ("GitHub response error (" <> show status <> ")")
        :# ["body" .= decodeUtf8 @Text body]
    Just (Aeson.Object km) ->
      "GitHub response error"
        :# map (uncurry (.=)) (KeyMap.toList km)
    Just val -> "GitHub response error" :# ["body" .= val]
 where
  status = statusCode $ getResponseStatus resp
  body = getResponseBody resp
