-- |
--
-- Module      : GHCTL.Variable
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Variable
  ( Variables (..)
  , Variable (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec

newtype Variables = Variables
  { variables :: [Variable]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Variable = Variable
  { name :: Text
  , value :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Variable)

instance HasCodec Variable where
  codec =
    object "Variable"
      $ Variable
      <$> (requiredField' "name" .= (.name))
      <*> (requiredField' "value" .= (.value))
