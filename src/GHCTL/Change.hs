-- |
--
-- Module      : GHCTL.Change
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change
  ( Change (..)
  , planChanges
  ) where

import GHCTL.Prelude

import Conduit
import Data.These.Combinators (bimapThese)
import GHCTL.BranchProtection
import GHCTL.RepositoriesYaml
import GHCTL.Repository
import GHCTL.Ruleset

data Change
  = CreateRepository Repository
  | DeleteRepository Repository
  | UpdateRepository Repository Repository
  | CreateBranchProtection Repository BranchProtection
  | DeleteBranchProtection Repository BranchProtection
  | UpdateBranchProtection Repository BranchProtection BranchProtection
  | CreateRuleset Repository Ruleset
  | DeleteRuleset Repository Ruleset
  | UpdateRuleset Repository Ruleset Ruleset
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON) -- logging

planChanges
  :: [RepositoriesYaml]
  -> [RepositoriesYaml]
  -> [Change]
planChanges as bs =
  concat
    $ runIdentity
    $ runConduit
    $ pairListOn (.repository.full_name) as bs
    .| sequenceSinks changeSinks

changeSinks
  :: Monad m
  => [ConduitT (These RepositoriesYaml RepositoriesYaml) Void m [Change]]
changeSinks =
  [ repositoryChanges .| sinkList
  , branchProtectionChanges .| sinkList
  , rulesetChanges .| sinkList
  ]

repositoryChanges
  :: Monad m
  => ConduitT (These RepositoriesYaml RepositoriesYaml) Change m ()
repositoryChanges =
  mapC (bimapThese (.repository) (.repository))
    .| theseToChanges CreateRepository DeleteRepository UpdateRepository

branchProtectionChanges
  :: Monad m
  => ConduitT (These RepositoriesYaml RepositoriesYaml) Change m ()
branchProtectionChanges = awaitForever $ \case
  This a -> traverse_ (yield . CreateBranchProtection a.repository) a.branch_protection
  That _ -> pure () -- nothing to do
  These a b ->
    pairMaybes a.branch_protection b.branch_protection
      .| theseToChanges
        (CreateBranchProtection a.repository)
        (DeleteBranchProtection a.repository)
        (UpdateBranchProtection a.repository)

rulesetChanges
  :: Monad m
  => ConduitT (These RepositoriesYaml RepositoriesYaml) Change m ()
rulesetChanges = awaitForever $ \case
  This a -> traverse_ (yield . CreateRuleset a.repository) a.rulesets
  That _ -> pure () -- nothing to do
  These a b ->
    pairListOn (.name) a.rulesets b.rulesets
      .| theseToChanges
        (CreateRuleset a.repository)
        (DeleteRuleset a.repository)
        (UpdateRuleset a.repository)

theseToChanges
  :: forall m a
   . (Eq a, Monad m)
  => (a -> Change)
  -- ^ create
  -> (a -> Change)
  -- ^ delete
  -> (a -> a -> Change)
  -- ^ update
  -> ConduitT (These a a) Change m ()
theseToChanges toCreate toDelete toUpdate = awaitForever $ \case
  This a -> yield $ toCreate a
  That b -> yield $ toDelete b
  These a b -> when (a /= b) $ yield $ toUpdate a b

pairMaybes
  :: Monad m
  => Maybe a
  -> Maybe b
  -> ConduitT i (These a b) m ()
pairMaybes = curry $ \case
  (Nothing, Nothing) -> pure ()
  (Just a, Nothing) -> yield $ This a
  (Nothing, Just b) -> yield $ That b
  (Just a, Just b) -> yield $ These a b

pairListOn
  :: forall m k a i
   . (Monad m, Ord k)
  => (a -> k)
  -> [a]
  -> [a]
  -> ConduitT i (These a a) m ()
pairListOn _ [] bs = yieldMany $ map That bs
pairListOn _ as [] = yieldMany $ map This as
pairListOn f (a : as) (b : bs) = case comparing f a b of
  EQ -> yield (These a b) >> pairListOn f as bs
  LT -> yield (This a) >> pairListOn f as (b : bs)
  GT -> yield (That b) >> pairListOn f (a : as) bs
