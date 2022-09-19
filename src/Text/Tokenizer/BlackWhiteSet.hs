{- |
  Module        : Text.Tokenizer.BlackWhiteSet
  Copyright     : (c) Lev Dvorkin, 2022
  License       : MIT
  Maintainer    : lev_135@mail.ru
  Stability     : Experimental

  This module contains auxillary set structure to store
  effectively small sets of symbols and complementary to them
-}
module Text.Tokenizer.BlackWhiteSet (
    BlackWhiteSet (..),
    singleton, intersection, isEmpty
  ) where

import Data.Set (Set)
import qualified Data.Set as S

-- | Select some "white set" of available elements or "black set" of
-- forbidden ones
data BlackWhiteSet c = BlackSet (Set c) | WhiteSet (Set c)
  deriving (Eq, Ord, Show)

-- | Make a 'BlackWhiteSet' containing only one symbol
singleton :: c -> BlackWhiteSet c
singleton = WhiteSet . S.singleton

-- | Intersect two 'BlackWhiteSet's.
intersection :: Ord c => BlackWhiteSet c -> BlackWhiteSet c -> BlackWhiteSet c
intersection (BlackSet b) (BlackSet b') = BlackSet (S.union b b')
intersection (BlackSet b) (WhiteSet w) = WhiteSet (S.difference w b)
intersection (WhiteSet w) (BlackSet b) = WhiteSet (S.difference w b)
intersection (WhiteSet w) (WhiteSet w') = WhiteSet (S.intersection w w')

-- | Check if 'BlackWhiteSet' is empty
--
-- NB! number of all elements assumed to be too large, so 'BlackSet' is never
--     supposed to be empty
isEmpty :: BlackWhiteSet c -> Bool
isEmpty (WhiteSet w) = null w
isEmpty _ = False
