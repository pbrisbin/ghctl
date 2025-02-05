module GHCTL.TextBoundedEnum
  ( TextBoundedEnum (..)
  ) where

import GHCTL.Prelude

import Data.Aeson (withText)
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)

fromTextBoundedEnum :: (Bounded a, Enum a, ToText a) => Text -> Either String a
fromTextBoundedEnum t = maybe (Left err) Right $ inverseMap toText t
 where
  err = "Unexpected value: " <> unpack t

newtype TextBoundedEnum a = TextBoundedEnum
  { unwrap :: a
  }
  deriving newtype (Bounded, Enum, ToText)

instance (Bounded a, Enum a, ToText a) => FromJSON (TextBoundedEnum a) where
  parseJSON = withText "" $ either fail pure . fromTextBoundedEnum

instance (Bounded a, Enum a, ToText a) => ToJSON (TextBoundedEnum a) where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

instance (Bounded a, Enum a, ToText a) => ToJSONKey (TextBoundedEnum a) where
  toJSONKey = toJSONKeyText toText
