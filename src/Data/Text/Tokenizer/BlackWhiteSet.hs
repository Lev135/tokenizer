module Data.Text.Tokenizer.BlackWhiteSet (
    BlackWhiteSet (..),
    bwsSingle, bwsIntersection, isEmpty
  ) where

import Data.Set (Set)
import qualified Data.Set as S

data BlackWhiteSet c = BlackSet (Set c) | WhiteSet (Set c)
  deriving (Eq, Ord, Show)

bwsSingle :: c -> BlackWhiteSet c
bwsSingle = WhiteSet . S.singleton

bwsIntersection :: Ord c => BlackWhiteSet c -> BlackWhiteSet c -> BlackWhiteSet c
bwsIntersection (BlackSet b) (BlackSet b') = BlackSet (S.union b b')
bwsIntersection (BlackSet b) (WhiteSet w) = WhiteSet (S.difference w b)
bwsIntersection (WhiteSet w) (BlackSet b) = WhiteSet (S.difference w b)
bwsIntersection (WhiteSet w) (WhiteSet w') = WhiteSet (S.intersection w w')

isEmpty :: BlackWhiteSet c -> Bool
isEmpty (WhiteSet w) = null w
isEmpty _ = False
