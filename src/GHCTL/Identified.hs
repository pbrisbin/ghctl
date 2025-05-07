-- |
--
-- Module      : GHCTL.Identified
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Identified
  ( Identified (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec

data Identified = Identified
  { id :: Int
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Identified)

instance HasCodec Identified where
  codec =
    object "Identified"
      $ Identified
      <$> (requiredField' "id" .= (.id))
      <*> (requiredField' "name" .= (.name))
