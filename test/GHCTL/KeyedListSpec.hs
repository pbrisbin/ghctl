-- |
--
-- Module      : GHCTL.KeyedListSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.KeyedListSpec
  ( spec
  ) where

import GHCTL.Prelude hiding ((.=))

import Autodocodec
import Data.Aeson.Encode.Pretty
import Data.Aeson.QQ
import GHCTL.KeyedList
import Test.Hspec

data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving stock (Eq, Generic, Show)

instance HasCodec Person where
  codec =
    object "Person"
      $ Person
      <$> (requiredField' "name" .= (.name))
      <*> (requiredField' "age" .= (.age))

newtype Store = Store
  { people :: KeyedList "name" Person
  }
  deriving stock (Eq, Generic, Show)

instance HasCodec Store where
  codec =
    object "Store"
      $ Store
      <$> (requiredField' "people" .= (.people))

spec :: Spec
spec = do
  describe "ToJSON" $ do
    it "matches the underlying type" $ do
      let
        store =
          Store
            { people =
                KeyedList
                  [ Person "bob" 45
                  , Person "alice" 39
                  ]
            }

        reference =
          [aesonQQ|
            {
              people: [
                { age: 45, name: "bob" },
                { age: 39, name: "alice" }
              ]
            }
          |]

      encodePretty (toJSONViaCodec store)
        `shouldBe` encodePretty (toJSONViaCodec reference)

  describe "FromJSON" $ do
    it "parses an array as-is" $ do
      let
        reference =
          [aesonQQ|
            {
              people: [
                { age: 45, name: "bob" },
                { age: 39, name: "alice" }
              ]
            }
          |]

        expected =
          Store
            { people =
                KeyedList
                  [ Person "bob" 45
                  , Person "alice" 39
                  ]
            }

      eitherDecodeJSONViaCodec (encodeJSONViaCodec reference)
        `shouldBe` Right expected

    it "parses objects keyed correctly" $ do
      let
        reference =
          [aesonQQ|
            {
              people: {
                bob: { age: 45 },
                alice: { age: 39 }
              }
            }
          |]

        -- N.B. parsing through object sorts the array by that key
        expected =
          Store
            { people =
                KeyedList
                  [ Person "alice" 39
                  , Person "bob" 45
                  ]
            }

      eitherDecodeJSONViaCodec (encodeJSONViaCodec reference)
        `shouldBe` Right expected
