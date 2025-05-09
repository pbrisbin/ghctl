-- |
--
-- Module      : GHCTL.RepositoryFullName
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.RepositoryFullName
  ( RepositoryFullName (..)
  , repositoryFullNameFromText
  ) where

import GHCTL.Prelude

import Autodocodec
import Data.Text qualified as T

data RepositoryFullName = RepositoryFullName
  { owner :: Text
  , name :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via (Autodocodec RepositoryFullName)

instance ToText RepositoryFullName where
  toText repo = repo.owner <> "/" <> repo.name

instance HasCodec RepositoryFullName where
  codec = bimapCodec repositoryFullNameFromText toText textCodec <?> "owner/name"

instance IsString RepositoryFullName where
  fromString = either (error . pack) id . repositoryFullNameFromText . pack

repositoryFullNameFromText :: Text -> Either String RepositoryFullName
repositoryFullNameFromText x = case T.splitOn "/" x of
  [owner, name] -> Right $ RepositoryFullName {owner, name}
  ps ->
    Left
      $ "Invalid repository full name: "
      <> unpack x
      <> ". Must be two /-separated parts, but got "
      <> show ps
