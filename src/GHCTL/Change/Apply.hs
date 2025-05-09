-- |
--
-- Module      : GHCTL.Change.Apply
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Change.Apply
  ( Delete
  , deleteOption
  , applyChange
  ) where

import GHCTL.Prelude

import GHCTL.CRUD (HasCRUD)
import GHCTL.CRUD qualified as CRUD
import GHCTL.Change
import GHCTL.GitHub (MonadGitHub)
import Options.Applicative (Parser, flag, help, long)

data Delete = Delete | Don'tDelete

deleteOption :: Parser Delete
deleteOption =
  flag Don'tDelete Delete
    $ mconcat
      [ long "no-skip-delete"
      , help "Don't skip changes that represent deletes"
      ]

applyChange :: (MonadGitHub m, MonadLogger m) => Delete -> Change -> m ()
applyChange d = \case
  ChangeRepository attr -> applyAttributeChange d attr
  ChangeBranchProtection attr -> applyAttributeChange d attr
  ChangeRuleset attr -> applyAttributeChange d attr
  ChangeVariable attr -> applyAttributeChange d attr

applyAttributeChange
  :: (HasCRUD a m, MonadLogger m) => Delete -> Attribute a -> m ()
applyAttributeChange d Attribute {repository, desiredCurrent} =
  case desiredCurrent of
    This a -> CRUD.create repository a
    That b -> case d of
      Delete -> CRUD.delete repository b
      Don'tDelete -> logWarn "Not deleting without --no-skip-delete"
    These a b -> CRUD.update repository a b
