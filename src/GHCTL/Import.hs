module GHCTL.Import
  ( importRepositories
  ) where

import GHCTL.Prelude

import Autodocodec (HasCodec, toJSONViaCodec)
import Blammo.Logging.ThreadContext (MonadMask, withThreadContext)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Align (align)
import Data.Map.Strict qualified as Map
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import GHCTL.Config
import GHCTL.GitHub (MonadGitHub)
import GHCTL.GitHub qualified as GitHub
import GHCTL.Options (ImportOptions (..))
import GHCTL.Repository
import GHCTL.RepositoryFullName
import GHCTL.RepositoryYaml
import GHCTL.RepositoryYaml.Fetch
import GHCTL.User qualified as GitHub
import Path.IO (doesFileExist)

importRepositories
  :: (MonadGitHub m, MonadIO m, MonadLogger m, MonadMask m)
  => ImportOptions
  -> Config
  -> m ()
importRepositories options config = do
  GitHub.User {login} <- GitHub.getUser
  repos <- GitHub.listUserRepositories login
  let toFullName = RepositoryFullName login . (.name)
  result <- foldMapM (importRepository options config . toFullName) repos
  logInfo
    $ "Imported repositories"
    :# [ "exists" .= result.exists
       , "skipped" .= result.skipped
       , "imported" .= result.imported
       ]

data ImportResult = ImportResult
  { exists :: Sum Int
  , skipped :: Sum Int
  , imported :: Sum Int
  }
  deriving stock (Generic)
  deriving (Monoid, Semigroup) via GenericSemigroupMonoid ImportResult

importRepository
  :: (MonadGitHub m, MonadIO m, MonadLogger m, MonadMask m)
  => ImportOptions
  -> Config
  -> RepositoryFullName
  -> m ImportResult
importRepository options config name = do
  withThreadContext ["full_name" .= name] $ do
    file <-
      either
        (logErrorDie . (:# []) . pack)
        (pure . (config.repositoriesDir </>))
        $ repositoryFullNameToFile name

    exists <- doesFileExist file

    if exists
      then pure $ mempty {exists = Sum 1}
      else do
        mCurrent <- getCurrentRepositoryYaml name

        case mCurrent of
          Just current | shouldImportRepository options current -> do
            let pruned = pruneDefaults config.defaults current
            logInfo $ "Import" :# ["path" .= file, "value" .= pruned]
            -- TODO: encode pruned as pretty-yaml to file
            pure $ mempty {imported = Sum 1}
          _ -> pure $ mempty {skipped = Sum 1}

shouldImportRepository :: ImportOptions -> RepositoryYaml -> Bool
shouldImportRepository options yaml
  | yaml.repository.archived && options.noArchived = False
  | yaml.repository.fork && options.source = False
  | otherwise = True

pruneDefaults :: HasCodec a => Value -> a -> Value
pruneDefaults defaults = go defaults . toJSONViaCodec
 where
  go :: Value -> Value -> Value
  go = curry $ \case
    (Object a, Object b) ->
      Object
        $ KeyMap.fromMap
        $ Map.fromList
        $ mapMaybe
          ( \(k, v) -> case v of
              This _ -> Nothing -- drop extra keys in defaults
              That y -> Just (k, y)
              These x y
                | x == y -> Nothing -- drop respecified defaults
                | otherwise -> Just (k, go x y) -- recurse
          )
        $ Map.toList
        $ align (KeyMap.toMap a) (KeyMap.toMap b)
    (_, b) -> b
