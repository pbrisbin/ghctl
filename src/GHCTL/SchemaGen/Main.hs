-- |
--
-- Module      : GHCTL.SchemaGen.Main
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.SchemaGen.Main
  ( main
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
import GHCTL.RepositoriesYaml

main :: IO ()
main = BSL.putStr $ encodePretty $ jsonSchemaViaCodec @RepositoryYaml

encodePretty :: ToJSON a => a -> BSL.ByteString
encodePretty =
  encodePretty'
    $ defConfig
      { confIndent = Spaces 2
      , confTrailingNewline = True
      }
