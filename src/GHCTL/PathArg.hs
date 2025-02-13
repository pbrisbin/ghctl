module GHCTL.PathArg
  ( PathArg (..)
  , readPathArg
  , showPathArg
  , getPathArgBytes
  ) where

import GHCTL.Prelude

import Data.ByteString qualified as BS
import Data.Functor.Alt ((<!>))
import Path (parseAbsFile, parseRelFile)

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
