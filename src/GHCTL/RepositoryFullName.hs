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
  , readRepositoryFullName
  ) where

import GHCTL.Prelude

import Data.Aeson (withText)
import Data.Text qualified as T

data RepositoryFullName = RepositoryFullName
  { owner :: Text
  , name :: Text
  }
  deriving stock (Eq, Ord, Show)

instance ToText RepositoryFullName where
  toText repo = repo.owner <> "/" <> repo.name

instance FromJSON RepositoryFullName where
  parseJSON =
    withText "RepositoryFullName"
      $ either fail pure
      . repositoryFullNameFromText

instance ToJSON RepositoryFullName where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

instance IsString RepositoryFullName where
  fromString = either (error . pack) id . repositoryFullNameFromText . pack

readRepositoryFullName :: String -> Either String RepositoryFullName
readRepositoryFullName = repositoryFullNameFromText . pack

repositoryFullNameFromText :: Text -> Either String RepositoryFullName
repositoryFullNameFromText x = case T.splitOn "/" x of
  [owner, name] -> Right $ RepositoryFullName {owner, name}
  ps ->
    Left
      $ "Invalid repository full name: "
      <> unpack x
      <> ". Must be two /-separated parts, but got "
      <> show ps
