-- |
--
-- Module      : GHCTL.Conduit
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Conduit
  ( module Conduit
  , sequenceSinks_
  , bothTheseC
  , catTheseMaybesC
  , pairTheseOnC
  , filterTheseC
  ) where

import GHCTL.Prelude

import Conduit
import Data.These.Combinators (bimapThese)

sequenceSinks_ :: Monad m => [ConduitT i o m ()] -> ConduitT i o m ()
sequenceSinks_ cs = awaitForever $ \i -> traverse_ (yield i .|) cs

bothTheseC :: Monad m => (a -> b) -> ConduitT (These a a) (These b b) m ()
bothTheseC f = mapC $ bimapThese f f

catTheseMaybesC
  :: Monad m => ConduitT (These (Maybe a) (Maybe b)) (These a b) m ()
catTheseMaybesC = concatMapC catTheseMaybes

catTheseMaybes :: These (Maybe a) (Maybe b) -> Maybe (These a b)
catTheseMaybes = \case
  This Nothing -> Nothing
  That Nothing -> Nothing
  This (Just a) -> Just $ This a
  That (Just b) -> Just $ That b
  These Nothing Nothing -> Nothing
  These (Just a) Nothing -> Just $ This a
  These Nothing (Just b) -> Just $ That b
  These (Just a) (Just b) -> Just $ These a b

pairTheseOnC
  :: forall m k a
   . (Monad m, Ord k)
  => (a -> k)
  -> ConduitT (These [a] [a]) (These a a) m ()
pairTheseOnC = awaitForever . pairTheseOn

pairTheseOn
  :: forall m k a i
   . (Monad m, Ord k)
  => (a -> k)
  -> These [a] [a]
  -> ConduitT i (These a a) m ()
pairTheseOn f = \case
  This as -> yieldMany $ map This as
  That bs -> yieldMany $ map This bs
  These as [] -> yieldMany $ map This as
  These [] bs -> yieldMany $ map That bs
  These (a : as) (b : bs) -> case comparing f a b of
    EQ -> yield (These a b) >> pairTheseOn f (These as bs)
    LT -> yield (This a) >> pairTheseOn f (These as (b : bs))
    GT -> yield (That b) >> pairTheseOn f (These (a : as) bs)

filterTheseC
  :: Monad m
  => (a -> b -> Bool)
  -> ConduitT (These a b) (These a b) m ()
filterTheseC f = concatMapC $ \case
  This a -> Just $ This a
  That b -> Just $ That b
  These a b | f a b -> Just $ These a b
  _ -> Nothing
