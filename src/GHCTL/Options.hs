{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : GHCTL.Options
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Options
  ( Options (..)
  , parseOptions
  ) where

import GHCTL.Prelude

import GHCTL.Change.Apply (Delete, deleteOption)
import GHCTL.PathArg
import GHCTL.RepositoryFullName
import Options.Applicative
import Path (relfile)

data Options = Options
  { path :: PathArg
  , apply :: Bool
  , delete :: Delete
  , failOnDiff :: Bool
  , failOnDiffExitCode :: Int
  , repositories :: Maybe (NonEmpty RepositoryFullName)
  }

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Maintain GitHub settings" optionsParser

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      (eitherReader readPathArg)
      ( mconcat
          [ short 'p'
          , long "path"
          , help "Path to repositories definition file"
          , metavar "FILE"
          , value (PathArgRel [relfile|./repositories.yaml|])
          , showDefaultWith showPathArg
          ]
      )
    <*> switch
      ( mconcat
          [ long "apply"
          , help "Apply changes to make current state look like desired"
          ]
      )
    <*> deleteOption
    <*> switch
      ( mconcat
          [ long "fail-on-diff"
          , help "Fail if there are un-applied differences"
          ]
      )
    <*> option
      auto
      ( mconcat
          [ long "fail-on-diff-exit-code"
          , help "Exit code for --fail-on-diff"
          , value 228
          , metavar "NUMBER"
          , showDefault
          ]
      )
    <*> (nonEmpty <$> many repositoryOption)

repositoryOption :: Parser RepositoryFullName
repositoryOption =
  argument (eitherReader $ repositoryFullNameFromText . pack)
    $ mconcat
      [ help "Limit processing to the given repositories"
      , metavar "OWNER/NAME"
      ]

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
