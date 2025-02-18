module GHCTL.Yaml
  ( decodeOptionalFile
  , decodeAllDefaults
  ) where

import GHCTL.Prelude

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml qualified as Yaml
import Path.IO (doesFileExist)

decodeOptionalFile
  :: forall m a b
   . (FromJSON a, MonadIO m, MonadLogger m)
  => a -> Path b File -> m a
decodeOptionalFile def path = do
  exists <- doesFileExist path

  if exists
    then decodeYaml fp =<< readFileBS fp
    else pure def
 where
  fp = toFilePath path

decodeAllDefaults
  :: forall m a b
   . (FromJSON a, MonadIO m, MonadLogger m) => Value -> Path b File -> m [a]
decodeAllDefaults defaults path = do
  bytes <- readFileBS fp
  values <- decodeAllYaml @_ @Value fp bytes
  traverse (decodeValue fp . mergeValues defaults) values
 where
  fp = toFilePath path

decodeValue
  :: forall m a. (FromJSON a, MonadIO m, MonadLogger m) => String -> Value -> m a
decodeValue path v = case fromJSON v of
  Error err -> do
    logError
      $ pack err
      :# [ "path" .= path
         , "input" .= v
         ]
    exitFailure
  Success a -> pure a

mergeValues :: Value -> Value -> Value
mergeValues = curry $ \case
  (Object a, Object b) -> Object $ KeyMap.unionWith mergeValues a b
  (_, v) -> v

decodeYaml
  :: forall m a
   . (FromJSON a, MonadIO m, MonadLogger m) => String -> ByteString -> m a
decodeYaml path bytes = case Yaml.decodeEither' bytes of
  Left ex -> dieYamlParseException path ex
  Right a -> pure a

decodeAllYaml
  :: forall m a
   . (FromJSON a, MonadIO m, MonadLogger m) => String -> ByteString -> m [a]
decodeAllYaml path bytes = case Yaml.decodeAllEither' bytes of
  Left ex -> dieYamlParseException path ex
  Right as -> pure as

dieYamlParseException
  :: forall m a. (MonadIO m, MonadLogger m) => String -> Yaml.ParseException -> m a
dieYamlParseException path ex = do
  logError $ message :# ["path" .= path]
  exitFailure
 where
  message :: Text
  message =
    "Exception decoding repositories file:\n"
      <> pack (Yaml.prettyPrintParseException ex)
