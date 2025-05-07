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

import Autodocodec
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, typeMismatch)
import Data.Vector qualified as V
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

newtype KeyedList (k :: Symbol) a = KeyedList
  { unwrap :: [a]
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON)
  deriving (FromJSON) via (Autodocodec (KeyedList k a))

instance (HasCodec a, KnownSymbol k) => HasCodec (KeyedList k a) where
  codec = parseAlternative keyedListCodec keyedObjectCodec

keyedListCodec :: HasCodec a => JSONCodec (KeyedList k a)
keyedListCodec = dimapCodec KeyedList (.unwrap) $ listCodec codec

keyedObjectCodec
  :: forall k a
   . (HasCodec a, KnownSymbol k)
  => JSONCodec (KeyedList k a)
keyedObjectCodec =
  bimapCodec fromValue toValue valueCodec
    <?> ( "An object of objects keyed by the value to use for their "
            <> key
            <> " property"
        )
 where
  key :: Text
  key = pack $ symbolVal $ Proxy @k

  fromValue :: Value -> Either String (KeyedList k a)
  fromValue = parseEither $ \case
    Object hm -> do
      as <-
        traverse
          ( \(k, v') -> case v' of
              Object hm' ->
                parseJSONViaCodec
                  $ Object
                  $ KeyMap.insert (Key.fromText key) (String $ Key.toText k) hm'
              _ -> typeMismatch "Object" v'
          )
          $ KeyMap.toList hm
      pure $ KeyedList as
    Array vs -> KeyedList <$> traverse parseJSONViaCodec (V.toList vs)
    v -> typeMismatch "Object or Array" v

  toValue :: KeyedList k a -> Value
  toValue = Array . V.fromList . map toJSONViaCodec . (.unwrap)
