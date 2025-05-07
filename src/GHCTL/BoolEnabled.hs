-- |
--
-- Module      : GHCTL.BoolEnabled
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.BoolEnabled
  ( BoolEnabled (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec

-- | A type representing an @enabled@ state
--
-- Canonically, this is an object with an @enabled@ key,
--
-- @
-- {
--   enabled: true|false
-- }
-- @
--
-- but we support parsing bare 'Bool' values as well.
--
-- @
-- true|false
-- @
newtype BoolEnabled = BoolEnabled
  { enabled :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec BoolEnabled)

instance HasCodec BoolEnabled where
  codec =
    dimapCodec BoolEnabled (.enabled)
      $ parseAlternative enabledObjectCodec boolCodec

enabledObjectCodec :: JSONCodec Bool
enabledObjectCodec = object "BoolEnabled" $ requiredField' "enabled" .= id
