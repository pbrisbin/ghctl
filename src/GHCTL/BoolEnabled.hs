module GHCTL.BoolEnabled
  ( BoolEnabled (..)
  ) where

import GHCTL.Prelude

import Data.Aeson (Value (..), (.:))
import Data.Aeson.Types (typeMismatch)

newtype BoolEnabled = BoolEnabled
  { enabled :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON)

instance FromJSON BoolEnabled where
  parseJSON = \case
    Object hm -> BoolEnabled <$> hm .: "enabled"
    Bool b -> pure $ BoolEnabled b
    v -> typeMismatch "Object with enabled key or Bool" v
