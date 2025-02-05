module GHCTL.RulesetEnforcement
  ( RulesetEnforcement (..)
  ) where

import GHCTL.Prelude

import GHCTL.TextBoundedEnum

data RulesetEnforcement
  = RulesetEnforcementDisabled
  | RulesetEnforcementActive
  | RulesetEnforcementEvaluate
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (TextBoundedEnum RulesetEnforcement)

instance ToText RulesetEnforcement where
  toText = \case
    RulesetEnforcementDisabled -> "disabled"
    RulesetEnforcementActive -> "active"
    RulesetEnforcementEvaluate -> "evaluate"
