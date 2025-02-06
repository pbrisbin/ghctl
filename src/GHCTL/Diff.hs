-- |
--
-- Module      : GHCTL.Diff
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GHCTL.Diff
  ( jsonDiff
  ) where

import GHCTL.Prelude

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Algorithm.Diff (Diff, getDiff)
import Data.Algorithm.Diff qualified as Diff
import Data.List.NonEmpty qualified as NE

jsonDiff :: (ToJSON a, ToJSON b) => a -> b -> [Diff Text]
jsonDiff a b = reduceContext $ getDiff (jsonLines a) (jsonLines b)

jsonLines :: ToJSON a => a -> [Text]
jsonLines = lines . decodeUtf8 . encodePretty

reduceContext :: [Diff Text] -> [Diff Text]
reduceContext = concatMap (toList . collapse) . NE.groupBy sameSide

collapse :: NonEmpty (Diff Text) -> NonEmpty (Diff Text)
collapse xs
  | Diff.Both {} <- head xs =
      let
        lenOmitted :: Natural
        lenOmitted = fromIntegral $ max 0 $ length xs - 2

        omittedText :: Text
        omittedText =
          "  ... "
            <> show lenOmitted
            <> " "
            <> pluralize lenOmitted "line" "lines"
            <> " omitted ..."
      in
        NE.head xs :| [Diff.Both omittedText omittedText, NE.last xs]
  | otherwise = xs

sameSide :: Diff a -> Diff a -> Bool
sameSide = curry $ \case
  (Diff.First {}, Diff.First {}) -> True
  (Diff.Second {}, Diff.Second {}) -> True
  (Diff.Both {}, Diff.Both {}) -> True
  _ -> False

pluralize :: Natural -> str -> str -> str
pluralize n singular plural = if n == 1 then singular else plural
