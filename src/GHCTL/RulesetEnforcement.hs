-- |
--
-- Module      : GHCTL.RulesetEnforcement
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RulesetEnforcement
  ( RulesetEnforcement (..)
  ) where

import GHCTL.Prelude

import Autodocodec
import GHCTL.TextBoundedEnum

data RulesetEnforcement
  = RulesetEnforcementDisabled
  | RulesetEnforcementActive
  | RulesetEnforcementEvaluate
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, HasCodec, ToJSON) via (TextBoundedEnum RulesetEnforcement)

instance ToText RulesetEnforcement where
  toText = \case
    RulesetEnforcementDisabled -> "disabled"
    RulesetEnforcementActive -> "active"
    RulesetEnforcementEvaluate -> "evaluate"
