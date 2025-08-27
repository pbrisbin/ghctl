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
  , repositoryFullNameFromFile
  , repositoryFullNameToFile
  ) where

import GHCTL.Prelude

import Autodocodec
import Data.Text qualified as T
import Path (addExtension, parseRelDir, parseRelFile, splitExtension)

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

repositoryFullNameFromFile :: Path Rel File -> Either String RepositoryFullName
repositoryFullNameFromFile path =
  case splitExtension path of
    Nothing -> Left $ extsErr "none"
    Just (_, ext) | ext `notElem` exts -> Left $ extsErr ext
    Just (base, _) -> repositoryFullNameFromText $ pack $ toFilePath base
 where
  exts :: [String]
  exts = [".yml", ".yaml"]

  extsErr :: String -> String
  extsErr x = "path must have valid extension (" <> show exts <> "), saw " <> x

repositoryFullNameToFile :: RepositoryFullName -> Either String (Path Rel File)
repositoryFullNameToFile name = first displayException $ do
  file <-
    (</>)
      <$> parseRelDir (unpack name.owner)
      <*> parseRelFile (unpack name.name)

  addExtension ".yaml" file
