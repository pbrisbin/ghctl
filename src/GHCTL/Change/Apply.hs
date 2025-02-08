-- |
--
-- Module      : GHCTL.Change.Apply
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change.Apply
  ( applyChange
  ) where

import GHCTL.Prelude

import GHCTL.CRUD (HasCRUD)
import GHCTL.CRUD qualified as CRUD
import GHCTL.Change
import GHCTL.GitHub (MonadGitHub)

applyChange :: (MonadGitHub m, MonadLogger m) => Change -> m ()
applyChange = \case
  ChangeRepository attr -> applyAttributeChange attr
  ChangeBranchProtection attr -> applyAttributeChange attr
  ChangeRuleset attr -> applyAttributeChange attr
  ChangeVariable attr -> applyAttributeChange attr

applyAttributeChange :: HasCRUD a m => Attribute a -> m ()
applyAttributeChange Attribute {repository, desiredCurrent} = case desiredCurrent of
  This a -> CRUD.create repository a
  That b -> CRUD.delete repository b
  These a b -> CRUD.update repository a b
