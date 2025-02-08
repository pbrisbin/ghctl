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
  , parseOptions
  ) where

import GHCTL.Prelude

import Data.List.NonEmpty (some1)
import GHCTL.PathArg
import GHCTL.RepositoryFullName
import Options.Applicative
import Path (relfile)

data Options = Options
  { path :: PathArg
  , command :: Command
  }

data Command
  = Plan Bool Int
  | Apply
  | Import (NonEmpty RepositoryFullName)

parseOptions :: IO Options
parseOptions = execParser $ withInfo "" optionsParser

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
    <*> subparser
      ( mconcat
          [ command "plan"
              . withInfo "show differences in desired and current state"
              $ planParser
          , command "apply"
              . withInfo "apply differences to current state"
              $ pure Apply
          , command "import"
              . withInfo "import repository definitions"
              $ importParser
          ]
      )

planParser :: Parser Command
planParser =
  Plan
    <$> switch
      ( mconcat
          [ long "fail-on-diff"
          , help "Fail if there are differences"
          ]
      )
    <*> option
      auto
      ( mconcat
          [ long "fail-on-diff-exit-code"
          , help "Exit code to use when failing due to diff"
          , value 228
          , showDefault
          ]
      )

importParser :: Parser Command
importParser =
  Import
    <$> some1
      ( argument (eitherReader readRepositoryFullName)
          $ mconcat
            [ help "Repository full name"
            , metavar "OWNER/REPO"
            ]
      )

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
