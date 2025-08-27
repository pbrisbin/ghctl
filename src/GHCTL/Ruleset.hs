-- |
--
-- Module      : GHCTL.Ruleset
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Ruleset
  ( Ruleset (..)
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec
import Data.Aeson (Value (..))
import Data.HashMap.Strict qualified as HashMap
import GHCTL.KeyedList
import GHCTL.RulesetEnforcement
import GHCTL.TextBoundedEnum

data Ruleset = Ruleset
  { name :: Text
  , target :: Text
  , enforcement :: RulesetEnforcement
  , bypass_actors :: [RulesetBypassActor]
  , conditions :: Maybe RulesetCondition
  , rules :: KeyedList "type" RulesetRule
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Ruleset)

instance HasCodec Ruleset where
  codec =
    object "Ruleset"
      $ Ruleset
      <$> (requiredField' "name" .= (.name))
      <*> (requiredField' "target" .= (.target))
      <*> (requiredField' "enforcement" .= (.enforcement))
      <*> (requiredField' "bypass_actors" .= (.bypass_actors))
      <*> (optionalField' "conditions" .= (.conditions))
      <*> (requiredField' "rules" .= (.rules))

data RulesetBypassActor = RulesetBypassActor
  { actor_type :: BypassActorType
  , actor_id :: Maybe Int
  , bypass_mode :: BypassMode
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RulesetBypassActor)

instance HasCodec RulesetBypassActor where
  codec =
    object "RulesetBypassActor"
      $ RulesetBypassActor
      <$> (requiredField' "actor_type" .= (.actor_type))
      <*> (optionalFieldOrNull' "actor_id" .= (.actor_id))
      <*> (requiredField' "bypass_mode" .= (.bypass_mode))

data BypassActorType
  = BypassActorTypeOrganizationAdmin
  | BypassActorTypeRepositoryAdmin
  | BypassActorTypeRepositoryRole
  | BypassActorTypeIntegration
  | BypassActorTypeTeam
  | BypassActorTypeDeployKey
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, HasCodec, ToJSON) via (TextBoundedEnum BypassActorType)

instance ToText BypassActorType where
  toText = \case
    BypassActorTypeOrganizationAdmin -> "OrganizationAdmin"
    BypassActorTypeRepositoryAdmin -> "RepositoryAdmin"
    BypassActorTypeRepositoryRole -> "RepositoryRole"
    BypassActorTypeIntegration -> "Integration"
    BypassActorTypeTeam -> "Team"
    BypassActorTypeDeployKey -> "DeployKey"

data BypassMode
  = BypassModeAlways
  | BypassModePullRequest
  | BypassModePullRequests
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, HasCodec, ToJSON) via (TextBoundedEnum BypassMode)

instance ToText BypassMode where
  toText = \case
    BypassModeAlways -> "always"
    BypassModePullRequest -> "pull_request"
    BypassModePullRequests -> "pull_requests"

newtype RulesetCondition = RulesetCondition
  { ref_name :: IncludeExclude
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RulesetCondition)

instance HasCodec RulesetCondition where
  codec =
    object "RulesetCondition"
      $ RulesetCondition
      <$> (requiredField' "ref_name" .= (.ref_name))

data IncludeExclude = IncludeExclude
  { include :: Maybe [Text]
  , exclude :: Maybe [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec IncludeExclude)

instance HasCodec IncludeExclude where
  codec =
    object "IncludeExclude"
      $ IncludeExclude
      <$> (optionalField' "include" .= (.include))
      <*> (optionalField' "exclude" .= (.exclude))

data RulesetRule
  = RulesetRule_creation
  | RulesetRule_update Value
  | RulesetRule_deletion
  | RulesetRule_required_linear_history
  | RulesetRule_merge_queue Value
  | RulesetRule_required_deployments Value
  | RulesetRule_required_signatures
  | RulesetRule_pull_request RulesetRulePullRequestParameters
  | RulesetRule_required_status_checks RulesetRuleRequiredStatusChecksParameters
  | RulesetRule_non_fast_forward
  | RulesetRule_commit_message_pattern Value
  | RulesetRule_commit_author_email_pattern Value
  | RulesetRule_committer_email_pattern Value
  | RulesetRule_branch_name_pattern Value
  | RulesetRule_tag_name_pattern Value
  | RulesetRule_file_path_restrictions Value
  | RulesetRule_max_file_path_length Value
  | RulesetRule_file_extension_restriction Value
  | RulesetRule_max_file_size Value
  | RulesetRule_workflows Value
  | RulesetRule_code_scanning Value
  deriving stock (Eq, Generic, Show)
{-# ANN module ("HLint: ignore Use camelCase" :: Text) #-}

instance HasCodec RulesetRule where
  codec =
    object "RulesetRule"
      $ discriminatedUnionCodec "type" enc dec
   where
    enc :: RulesetRule -> (Discriminator, ObjectCodec RulesetRule ())
    enc = \case
      RulesetRule_creation -> ("creation", pureCodec ())
      RulesetRule_update p -> ("update", mapToEncoder p parametersCodec)
      RulesetRule_deletion -> ("deletion", pureCodec ())
      RulesetRule_required_linear_history -> ("required_linear_history", pureCodec ())
      RulesetRule_merge_queue p -> ("merge_queue", mapToEncoder p parametersCodec)
      RulesetRule_required_deployments p -> ("required_deployments", mapToEncoder p parametersCodec)
      RulesetRule_required_signatures -> ("required_signatures", pureCodec ())
      RulesetRule_pull_request p -> ("pull_request", mapToEncoder p parametersCodec)
      RulesetRule_required_status_checks p -> ("required_status_checks", mapToEncoder p parametersCodec)
      RulesetRule_non_fast_forward -> ("non_fast_forward", pureCodec ())
      RulesetRule_commit_message_pattern p -> ("commit_message_pattern", mapToEncoder p parametersCodec)
      RulesetRule_commit_author_email_pattern p -> ("commit_author_email_pattern", mapToEncoder p parametersCodec)
      RulesetRule_committer_email_pattern p -> ("committer_email_pattern", mapToEncoder p parametersCodec)
      RulesetRule_branch_name_pattern p -> ("branch_name_pattern", mapToEncoder p parametersCodec)
      RulesetRule_tag_name_pattern p -> ("tag_name_pattern", mapToEncoder p parametersCodec)
      RulesetRule_file_path_restrictions p -> ("file_path_restrictions", mapToEncoder p parametersCodec)
      RulesetRule_max_file_path_length p -> ("max_file_path_length", mapToEncoder p parametersCodec)
      RulesetRule_file_extension_restriction p -> ("file_extension_restriction", mapToEncoder p parametersCodec)
      RulesetRule_max_file_size p -> ("max_file_size", mapToEncoder p parametersCodec)
      RulesetRule_workflows p -> ("workflows", mapToEncoder p parametersCodec)
      RulesetRule_code_scanning p -> ("code_scanning", mapToEncoder p parametersCodec)

    dec :: HashMap Discriminator (Text, ObjectCodec Void RulesetRule)
    dec =
      HashMap.fromList
        [ ("creation", ("RulesetRule_creation", pureCodec RulesetRule_creation))
        ,
          ( "update"
          , ("RulesetRule_update", mapToDecoder RulesetRule_update parametersCodec)
          )
        , ("deletion", ("RulesetRule_deletion", pureCodec RulesetRule_deletion))
        ,
          ( "required_linear_history"
          ,
            ( "RulesetRule_required_linear_history"
            , pureCodec RulesetRule_required_linear_history
            )
          )
        ,
          ( "merge_queue"
          ,
            ( "RulesetRule_merge_queue"
            , mapToDecoder RulesetRule_merge_queue parametersCodec
            )
          )
        ,
          ( "required_deployments"
          ,
            ( "RulesetRule_required_deployments"
            , mapToDecoder RulesetRule_required_deployments parametersCodec
            )
          )
        ,
          ( "required_signatures"
          , ("RulesetRule_required_signatures", pureCodec RulesetRule_required_signatures)
          )
        ,
          ( "pull_request"
          ,
            ( "RulesetRule_pull_request"
            , mapToDecoder RulesetRule_pull_request parametersCodec
            )
          )
        ,
          ( "required_status_checks"
          ,
            ( "RulesetRule_required_status_checks"
            , mapToDecoder RulesetRule_required_status_checks parametersCodec
            )
          )
        ,
          ( "non_fast_forward"
          , ("RulesetRule_non_fast_forward", pureCodec RulesetRule_non_fast_forward)
          )
        ,
          ( "commit_message_pattern"
          ,
            ( "RulesetRule_commit_message_pattern"
            , mapToDecoder RulesetRule_commit_message_pattern parametersCodec
            )
          )
        ,
          ( "commit_author_email_pattern"
          ,
            ( "RulesetRule_commit_author_email_pattern"
            , mapToDecoder RulesetRule_commit_author_email_pattern parametersCodec
            )
          )
        ,
          ( "committer_email_pattern"
          ,
            ( "RulesetRule_committer_email_pattern"
            , mapToDecoder RulesetRule_committer_email_pattern parametersCodec
            )
          )
        ,
          ( "branch_name_pattern"
          ,
            ( "RulesetRule_branch_name_pattern"
            , mapToDecoder RulesetRule_branch_name_pattern parametersCodec
            )
          )
        ,
          ( "tag_name_pattern"
          ,
            ( "RulesetRule_tag_name_pattern"
            , mapToDecoder RulesetRule_tag_name_pattern parametersCodec
            )
          )
        ,
          ( "file_path_restrictions"
          ,
            ( "RulesetRule_file_path_restrictions"
            , mapToDecoder RulesetRule_file_path_restrictions parametersCodec
            )
          )
        ,
          ( "max_file_path_length"
          ,
            ( "RulesetRule_max_file_path_length"
            , mapToDecoder RulesetRule_max_file_path_length parametersCodec
            )
          )
        ,
          ( "file_extension_restriction"
          ,
            ( "RulesetRule_file_extension_restriction"
            , mapToDecoder RulesetRule_file_extension_restriction parametersCodec
            )
          )
        ,
          ( "max_file_size"
          ,
            ( "RulesetRule_max_file_size"
            , mapToDecoder RulesetRule_max_file_size parametersCodec
            )
          )
        ,
          ( "workflows"
          , ("RulesetRule_workflows", mapToDecoder RulesetRule_workflows parametersCodec)
          )
        ,
          ( "code_scanning"
          ,
            ( "RulesetRule_code_scanning"
            , mapToDecoder RulesetRule_code_scanning parametersCodec
            )
          )
        ]

    parametersCodec = requiredField' "parameters"

data RulesetRulePullRequestParameters = RulesetRulePullRequestParameters
  { allowed_merge_methods :: [MergeMethod]
  , dismiss_stale_reviews_on_push :: Bool
  , require_code_owner_review :: Bool
  , require_last_push_approval :: Bool
  , required_approving_review_count :: Int
  , required_review_thread_resolution :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RulesetRulePullRequestParameters)

instance HasCodec RulesetRulePullRequestParameters where
  codec =
    object "RulesetRulePullRequestParameters"
      $ RulesetRulePullRequestParameters
      <$> (requiredField' "allowed_merge_methods" .= (.allowed_merge_methods))
      <*> ( requiredField' "dismiss_stale_reviews_on_push"
              .= (.dismiss_stale_reviews_on_push)
          )
      <*> (requiredField' "require_code_owner_review" .= (.require_code_owner_review))
      <*> (requiredField' "require_last_push_approval" .= (.require_last_push_approval))
      <*> ( requiredField' "required_approving_review_count"
              .= (.required_approving_review_count)
          )
      <*> ( requiredField' "required_review_thread_resolution"
              .= (.required_review_thread_resolution)
          )

data MergeMethod
  = MergeMethodMerge
  | MergeMethodSquash
  | MergeMethodRebase
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromJSON, HasCodec, ToJSON) via (TextBoundedEnum MergeMethod)

instance ToText MergeMethod where
  toText = \case
    MergeMethodMerge -> "merge"
    MergeMethodSquash -> "squash"
    MergeMethodRebase -> "rebase"

data RulesetRuleRequiredStatusChecksParameters = RulesetRuleRequiredStatusChecksParameters
  { required_status_checks :: KeyedList "context" RequiredStatusCheck
  , strict_required_status_checks_policy :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via (Autodocodec RulesetRuleRequiredStatusChecksParameters)

instance HasCodec RulesetRuleRequiredStatusChecksParameters where
  codec =
    object "RulesetRuleRequiredStatusChecksParameters"
      $ RulesetRuleRequiredStatusChecksParameters
      <$> (requiredField' "required_status_checks" .= (.required_status_checks))
      <*> ( requiredField' "strict_required_status_checks_policy"
              .= (.strict_required_status_checks_policy)
          )

data RequiredStatusCheck = RequiredStatusCheck
  { context :: Text
  , integration_id :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec RequiredStatusCheck)

instance HasCodec RequiredStatusCheck where
  codec =
    object "RequiredStatusCheck"
      $ RequiredStatusCheck
      <$> (requiredField' "context" .= (.context))
      <*> (optionalField' "integration_id" .= (.integration_id))
