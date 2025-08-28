{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : GHCTL.SchemaGen
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Docs
  ( updateDocs
  ) where

import GHCTL.Prelude

import Data.Text.IO qualified as T
import GHCTL.Options
import Options.Applicative (Parser)
import Path (parent, parseRelFile, reldir)
import Path.IO (createDirIfMissing)
import Ronn
  ( ManRef (..)
  , ManSection (..)
  , Ronn (..)
  , getSections
  , ronnFilePath
  , ronnToText
  )
import Ronn.AST qualified as Ronn
import Ronn.Options.Applicative ()

updateDocs :: IO ()
updateDocs = do
  -- writeRonn $ ghctlRonn "ghctl" optionsInfo optionsParser
  writeRonn $ ghctlRonn "ghctl-plan" planCmdInfo planOptionsParser
  writeRonn $ ghctlRonn "ghctl-apply" applyCmdInfo applyOptionsParser
  writeRonn $ ghctlRonn "ghctl-import" importCmdInfo importOptionsParser
  writeRonn
    $ Ronn
      { name = ManRef "ghctl" ManSection5
      , description = [".ghctl directory"]
      , sections = []
      }
 where
  ghctlRonn :: Text -> Ronn.Part -> Parser a -> Ronn
  ghctlRonn name descr p =
    Ronn
      { name = ManRef name ManSection1
      , description = [descr]
      , sections = getSections name p
      }

writeRonn :: Ronn -> IO ()
writeRonn ronn = do
  createDirIfMissing True $ parent path
  T.writeFile (toFilePath path) $ ronnToText ronn
 where
  file = parseRelFileUnsafe $ ronnFilePath ronn
  path = [reldir|docs|] </> file

parseRelFileUnsafe :: FilePath -> Path Rel File
parseRelFileUnsafe = either (error . pack . displayException) id . parseRelFile
