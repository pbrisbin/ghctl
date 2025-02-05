module GHCTL.Identified
  ( Identified (..)
  ) where

import GHCTL.Prelude

data Identified = Identified
  { id :: Int
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
