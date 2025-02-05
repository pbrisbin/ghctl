module GHCTL.Ruleset
  ( Ruleset (..)
  ) where

import GHCTL.Prelude

import Data.Aeson (Value, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson qualified as Aeson
import Data.List.Extra (dropPrefix)
import GHCTL.RulesetEnforcement
import GHCTL.TextBoundedEnum

data Ruleset = Ruleset
  { name :: Text
  , target :: Text
  , enforcement :: RulesetEnforcement
  , bypass_actors :: [RulesetBypassActor]
  , conditions :: Maybe RulesetCondition
  , rules :: [RulesetRule]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RulesetBypassActor = RulesetBypassActor
  { actor_type :: BypassActorType
  , actor_id :: Maybe Value
  , bypass_mode :: BypassMode
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data BypassActorType
  = BypassActorTypeOrganizationAdmin
  | BypassActorTypeRepositoryAdmin
  | BypassActorTypeRepositoryRole
  | BypassActorTypeIntegration
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (TextBoundedEnum BypassActorType)

instance ToText BypassActorType where
  toText = \case
    BypassActorTypeOrganizationAdmin -> "OrganizationAdmin"
    BypassActorTypeRepositoryAdmin -> "RepositoryAdmin"
    BypassActorTypeRepositoryRole -> "RepositoryRole"
    BypassActorTypeIntegration -> "Integration"

data BypassMode
  = BypassModeAlways
  | BypassModePullRequest
  | BypassModePullRequests
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (TextBoundedEnum BypassMode)

instance ToText BypassMode where
  toText = \case
    BypassModeAlways -> "always"
    BypassModePullRequest -> "pull_request"
    BypassModePullRequests -> "pull_requests"

newtype RulesetCondition = RulesetCondition
  { ref_name :: IncludeExclude
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data IncludeExclude = IncludeExclude
  { include :: Maybe [Text]
  , exclude :: Maybe [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RulesetRule
  = RulesetRule_deletion
  | RulesetRule_pull_request RulesetRulePullRequestParameters
  | RulesetRule_required_status_checks RulesetRuleRequiredStatusChecksParameters
  | RulesetRule_non_fast_forward
  deriving stock (Eq, Generic, Show)
{-# ANN module ("HLint: ignore Use camelCase" :: Text) #-}

instance FromJSON RulesetRule where
  parseJSON = genericParseJSON ruleAesonOptions

instance ToJSON RulesetRule where
  toJSON = genericToJSON ruleAesonOptions
  toEncoding = genericToEncoding ruleAesonOptions

ruleAesonOptions :: Aeson.Options
ruleAesonOptions =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = dropPrefix "RulesetRule_"
    , Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type"
          , Aeson.contentsFieldName = "parameters"
          }
    }

data RulesetRulePullRequestParameters = RulesetRulePullRequestParameters
  { allowed_merge_methods :: [MergeMethod]
  , dismiss_stale_reviews_on_push :: Bool
  , require_code_owner_review :: Bool
  , require_last_push_approval :: Bool
  , required_approving_review_count :: Int
  , required_review_thread_resolution :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data MergeMethod
  = MergeMethodMerge
  | MergeMethodSquash
  | MergeMethodRebase
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (TextBoundedEnum MergeMethod)

instance ToText MergeMethod where
  toText = \case
    MergeMethodMerge -> "merge"
    MergeMethodSquash -> "squash"
    MergeMethodRebase -> "rebase"

data RulesetRuleRequiredStatusChecksParameters = RulesetRuleRequiredStatusChecksParameters
  { required_status_checks :: [RequiredStatusCheck]
  , strict_required_status_checks_policy :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RequiredStatusCheck = RequiredStatusCheck
  { context :: Text
  , integration_id :: Maybe Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
