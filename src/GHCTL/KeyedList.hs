-- |
--
-- Module      : GHCTL.KeyedList
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.KeyedList
  ( KeyedList (..)
  ) where

import GHCTL.Prelude

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

newtype KeyedList (k :: Symbol) a = KeyedList
  { unwrap :: [a]
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON)

instance (FromJSON a, KnownSymbol k) => FromJSON (KeyedList k a) where
  parseJSON = \case
    Object hm -> do
      as <-
        traverse
          ( \(k, v') -> case v' of
              Object hm' -> do
                let
                  key :: Key
                  key = Key.fromString $ symbolVal $ Proxy @k

                  val :: Value
                  val = Object $ KeyMap.insert key (String $ Key.toText k) hm'

                parseJSON val
              _ -> fail "TODO"
          )
          $ KeyMap.toList hm
      pure $ KeyedList as
    v -> KeyedList <$> parseJSON v
