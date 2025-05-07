-- |
--
-- Module      : GHCTL.TextBoundedEnum
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.TextBoundedEnum
  ( TextBoundedEnum (..)
  ) where

import GHCTL.Prelude

import Autodocodec
import Data.Aeson (withText)
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)

fromTextBoundedEnum :: (Bounded a, Enum a, ToText a) => Text -> Either String a
fromTextBoundedEnum t = maybe (Left err) Right $ inverseMap toText t
 where
  err = "Unexpected value: " <> unpack t

newtype TextBoundedEnum a = TextBoundedEnum
  { unwrap :: a
  }
  deriving newtype (Bounded, Enum, Eq, ToText)

instance (Bounded a, Enum a, ToText a) => FromJSON (TextBoundedEnum a) where
  parseJSON = withText "" $ either fail pure . fromTextBoundedEnum

instance (Bounded a, Enum a, ToText a) => ToJSON (TextBoundedEnum a) where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

instance (Bounded a, Enum a, ToText a) => ToJSONKey (TextBoundedEnum a) where
  toJSONKey = toJSONKeyText toText

instance (Bounded a, Enum a, Eq a, ToText a) => HasCodec (TextBoundedEnum a) where
  codec = boundedEnumCodec toText
