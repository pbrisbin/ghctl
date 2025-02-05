{-# LANGUAGE QuasiQuotes #-}

module GHCTL.Options
  ( Options (..)
  , Command (..)
  , PathArg (..)
  , getPathArgBytes
  , appendPathArgBytes
  , parseOptions
  ) where

import GHCTL.Prelude

import Data.ByteString qualified as BS
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (some1)
import GHCTL.RepositoryFullName
import Options.Applicative
import Path (parseAbsFile, parseRelFile, relfile)

data Options = Options
  { path :: PathArg
  , command :: Command
  }

data Command
  = Plan
  | Apply
  | Import (NonEmpty RepositoryFullName)

data PathArg
  = PathArgAbs (Path Abs File)
  | PathArgRel (Path Rel File)
  | PathArgStdin

readPathArg :: String -> Either String PathArg
readPathArg = \case
  "-" -> Right PathArgStdin
  p -> readPathArgAbs p <!> readPathArgRel p

readPathArgAbs :: String -> Either String PathArg
readPathArgAbs p = PathArgAbs <$> first show (parseAbsFile p)

readPathArgRel :: String -> Either String PathArg
readPathArgRel p = PathArgRel <$> first show (parseRelFile p)

showPathArg :: PathArg -> String
showPathArg = \case
  PathArgAbs p -> toFilePath p
  PathArgRel p -> toFilePath p
  PathArgStdin -> "-"

getPathArgBytes :: MonadIO m => PathArg -> m ByteString
getPathArgBytes = \case
  PathArgAbs p -> readFileBS $ toFilePath p
  PathArgRel p -> readFileBS $ toFilePath p
  PathArgStdin -> liftIO BS.getContents

appendPathArgBytes :: MonadIO m => PathArg -> ByteString -> m ()
appendPathArgBytes = \case
  PathArgAbs p -> appendFileBS (toFilePath p)
  PathArgRel p -> appendFileBS (toFilePath p)
  PathArgStdin -> liftIO . BS.putStr

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
              $ pure Plan
          , command "apply"
              . withInfo "apply differences to current state"
              $ pure Apply
          , command "import"
              . withInfo "import repository definitions"
              $ importParser
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
