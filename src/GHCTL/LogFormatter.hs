-- |
--
-- Module      : GHCTL.LogFormatter
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.LogFormatter
  ( reformatLoggedMessage
  ) where

import GHCTL.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.LogSettings (LogSettings)
import Control.Monad.Logger.Aeson
import Data.Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T

reformatLoggedMessage
  :: LogSettings -> Colors -> LogLevel -> LoggedMessage -> ByteString
reformatLoggedMessage _ Colors {..} logLevel LoggedMessage {..} =
  encodeUtf8
    $ mconcat
      [ case logLevel of
          LevelDebug -> blue "DEBUG:"
          LevelInfo -> green " INFO:"
          LevelWarn -> yellow " WARN:"
          LevelError -> red "ERROR:"
          LevelOther x -> gray $ T.take 5 x <> ":"
      , " " <> loggedMessageText
      , maybe "" (("\n" <>) . dim) $ do
          guard $ not $ KeyMap.null metaKeyMap
          pure $ encodePretty $ Object metaKeyMap
      ]
 where
  metaKeyMap =
    maybe mempty (KeyMap.singleton "source" . String) loggedMessageLogSource
      <> loggedMessageThreadContext
      <> loggedMessageMeta

encodePretty :: Value -> Text
encodePretty =
  (indent <>)
    . T.intercalate ("\n" <> indent)
    . T.lines
    . decodeUtf8
    . BSL.toStrict
    . Pretty.encodePretty' conf
 where
  indent = T.replicate 7 " "
  conf = Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}
