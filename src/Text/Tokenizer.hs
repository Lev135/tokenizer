{- |
  Module        : Text.Tokenizer
  Copyright     : (c) Lev Dvorkin, 2022
  License       : MIT
  Maintainer    : lev_135@mail.ru
  Stability     : Experimental

  This module reexports everything you need from the package
-}
module Text.Tokenizer (
    BlackWhiteSet (..), bwsSingle, bwsIntersection,
    Count (..), Repeatable (..),
    Token (..),

    ConflictTokens (..), checkUniqueTokenizing
  ) where

import Text.Tokenizer.BlackWhiteSet
  (BlackWhiteSet(..), bwsSingle, bwsIntersection)
import Text.Tokenizer.Types
    (Count (..), Token(..), Repeatable(..), getBWS)
import Text.Tokenizer.Uniqueness
  (ConflictTokens (..), checkUniqueTokenizing)
