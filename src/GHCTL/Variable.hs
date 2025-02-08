module GHCTL.Variable
  ( Variables (..)
  , Variable (..)
  ) where

import GHCTL.Prelude

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
  deriving anyclass (FromJSON, ToJSON)
