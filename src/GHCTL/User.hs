-- |
--
-- Module      : GHCTL.User
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.User
  ( User (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec

newtype User = User
  { login :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec User)

instance HasCodec User where
  codec = object "User" $ User <$> (requiredField' "login" .= (.login))
