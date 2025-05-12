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
  , Command (..)
  , PlanOptions (..)
  , ApplyOptions (..)
  , parseOptions
  ) where

import GHCTL.Prelude

import GHCTL.Change.Apply (Delete, deleteOption)
import GHCTL.RepositoryFullName
import Options.Applicative
import Path (SomeBase (..), parseSomeDir, reldir)

data Options = Options
  { dir :: SomeBase Dir
  , command :: Command
  }

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Maintain GitHub settings" optionsParser

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      (eitherReader $ first show . parseSomeDir)
      ( mconcat
          [ short 'd'
          , long "dir"
          , help "Path to GHCTL directory"
          , metavar "DIRECTORY"
          , value (Rel [reldir|./.ghctl|])
          , showDefault
          ]
      )
    <*> commandParser

data Command
  = Plan PlanOptions
  | Apply ApplyOptions
  | Schema

commandParser :: Parser Command
commandParser =
  subparser
    $ mconcat
      [ command "plan"
          $ withInfo "Show differences between desired and current"
          $ Plan
          <$> planOptionsParser
      , command "apply"
          $ withInfo "Apply changes to make current match desired"
          $ Apply
          <$> applyOptionsParser
      , command "schema" $ withInfo "Dump configuration schema" $ pure Schema
      ]

data PlanOptions = PlanOptions
  { failOnDiff :: Bool
  , failOnDiffExitCode :: Int
  , repositories :: Maybe (NonEmpty RepositoryFullName)
  }

planOptionsParser :: Parser PlanOptions
planOptionsParser =
  PlanOptions
    <$> switch
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

data ApplyOptions = ApplyOptions
  { delete :: Delete
  , repositories :: Maybe (NonEmpty RepositoryFullName)
  }

applyOptionsParser :: Parser ApplyOptions
applyOptionsParser =
  ApplyOptions
    <$> deleteOption
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
