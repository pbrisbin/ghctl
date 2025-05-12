-- |
--
-- Module      : GHCTL.SchemaGen
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.SchemaGen
  ( prettySchema
  ) where

import GHCTL.Prelude

import Autodocodec.Schema
import Data.Aeson.Encode.Pretty
  ( Config (..)
  , Indent (..)
  , defConfig
  , encodePretty'
  )
import Data.ByteString.Lazy qualified as BSL
import GHCTL.RepositoryYaml

prettySchema :: Text
prettySchema = decodeUtf8 $ encodePretty $ jsonSchemaViaCodec @RepositoryYaml

encodePretty :: ToJSON a => a -> BSL.ByteString
encodePretty = encodePretty' $ defConfig {confIndent = Spaces 2}
