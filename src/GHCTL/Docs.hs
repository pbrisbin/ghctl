{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : GHCTL.Docs
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
  , seeAlsoSection
  )
import Ronn.AST qualified as Ronn
import Ronn.Options.Applicative ()

updateDocs :: IO ()
updateDocs = do
  -- writeRonn $ ghctlRonn "ghctl" optionsInfo optionsParser
  writeRonn
    $ ghctlRonn
      "ghctl-plan"
      planCmdInfo
      planOptionsParser
      [ seeAlsoSection
          [ ManRef "ghctl" ManSection1
          , ManRef "ghctl" ManSection5
          , ManRef "ghctl-apply" ManSection1
          ]
      ]
  writeRonn
    $ ghctlRonn
      "ghctl-apply"
      applyCmdInfo
      applyOptionsParser
      [ seeAlsoSection
          [ ManRef "ghctl" ManSection1
          , ManRef "ghctl" ManSection5
          , ManRef "ghctl-plan" ManSection1
          ]
      ]
  writeRonn
    $ ghctlRonn
      "ghctl-import"
      importCmdInfo
      importOptionsParser
      [ seeAlsoSection
          [ ManRef "ghctl" ManSection1
          , ManRef "ghctl" ManSection5
          ]
      ]
  writeRonn
    $ Ronn
      { name = ManRef "ghctl" ManSection5
      , description = [".ghctl directory"]
      , sections =
          [ Ronn.Section
              { Ronn.name = "DESCRIPTION"
              , Ronn.content =
                  [ paragraph
                      [ Ronn.Strong "ghctl"
                      , "operates on a directory structure to determine the"
                      , "desired state of GitHub repositories. This page"
                      , "describes the expected layout and schema."
                      ]
                  ]
              }
          , Ronn.Section
              { Ronn.name = "REPOSITORIES"
              , Ronn.content =
                  [ paragraph
                      [ "The"
                      , Ronn.Code "repositories"
                      , "directory is expected to contain 2-levels of directory. Files"
                      , "will be interpreted as"
                      , Ronn.Code "{owner}/{name}.yaml" <> ","
                      , "and their contents represent their desired state to"
                      , "verify and/or apply."
                      ]
                  ]
              }
          , Ronn.Section
              { Ronn.name = "DEFAULTS"
              , Ronn.content =
                  [ paragraph
                      [ "If present, the"
                      , Ronn.Code "defaults.yaml"
                      , "file will be loaded and applied to each repository file"
                      , "before processing it. This step occurs as schema-less"
                      , "values, recursively merging objects and biasing towards"
                      , "the repository side."
                      ]
                  ]
              }
          , Ronn.Section
              { Ronn.name = "SCHEMA"
              , Ronn.content =
                  [ paragraph
                      [ "The repository schema can be found [here][schema]. However, this schema is only"
                      , "asserted"
                      , Ronn.Strong "after defaults are applied" <> "."
                      , "Therefore, any given repository file itself need not be"
                      , "valid schema. For example, managing a repository that"
                      , "follows all defined defaults would simply be a file at"
                      , "the desired name and with"
                      , Ronn.Code "{}"
                      , "as its contents."
                      ]
                  , paragraph
                      ["[schema]: ./repository.schema.json"]
                  , paragraph
                      [ "Our schema follows GitHub's API exactly, so feel free to go by that"
                      , "documentation as well. There is a minor difference when"
                      , "it comes to \"keyed\" lists"
                      ]
                  , paragraph
                      ["For example,", Ronn.Code "rulesets", "might look like:"]
                  , codeblock
                      "yaml"
                      [ "rulesets:"
                      , "  - name: main"
                      , "    # {more properties}"
                      ]
                  , paragraph
                      [ "This is how the object will be encoded when interacting with the GitHub API and"
                      , "is supported by our schema. However, we also support decoding the following:"
                      ]
                  , codeblock
                      "yaml"
                      [ "rulesets:"
                      , "  main:"
                      , "    # {more properties}"
                      ]
                  , paragraph
                      [ "This makes it possible to supply defaults within the sub-object at the key"
                      , Ronn.Code "main" <> "."
                      , "Otherwise, wanting to change any attribute of any rule"
                      , "would require respecifying the entire"
                      , Ronn.Code "rulesets"
                      , "list."
                      ]
                  , paragraph
                      [ "We support this decoding anywhere there is a clear"
                      , "uniquely-identifying property, such as"
                      , Ronn.Code "name" <> ","
                      , Ronn.Code "type" <> ","
                      , "or"
                      , Ronn.Code "key" <> "."
                      ]
                  ]
              }
          , seeAlsoSection [ManRef "ghctl" ManSection1]
          ]
      }

ghctlRonn :: Text -> Ronn.Part -> Parser a -> [Ronn.Section] -> Ronn
ghctlRonn name descr p extra =
  Ronn
    { name = ManRef name ManSection1
    , description = [descr]
    , sections = getSections name p <> extra
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

paragraph :: [Ronn.Part] -> Ronn.Content
paragraph = Ronn.Groups . pure . Ronn.Lines . pure . Ronn.Line

codeblock :: Text -> [Text] -> Ronn.Content
codeblock lang =
  Ronn.Groups
    . pure
    . Ronn.Lines
    . map (Ronn.Line . pure . Ronn.Raw)
    . (("```" <> lang) :)
    . (<> ["```"])
