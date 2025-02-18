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

import Options.Applicative
import Path (reldir)

data Options = Options
  { dir :: Path Rel Dir
  , command :: Command
  }

data Command
  = Plan Bool Int
  | Apply

parseOptions :: IO Options
parseOptions = execParser $ withInfo "" optionsParser

optionsParser :: Parser Options
optionsParser =
  Options [reldir|.|]
    <$> subparser
      ( mconcat
          [ command "plan"
              . withInfo "show differences in desired and current state"
              $ planParser
          , command "apply"
              . withInfo "apply differences to current state"
              $ pure Apply
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

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
