{- |
  Module        : Text.Tokenizer
  Copyright     : (c) Lev Dvorkin, 2022
  License       : MIT
  Maintainer    : lev_135@mail.ru
  Stability     : Experimental

  This module reexports everything you need from the package
-}
module Text.Tokenizer (
    -- * Structures for tokens representation
    BlackWhiteSet (..),
    Count (..), Repeatable (..),
    Token (..),

    -- * Uniqueness checking
    ConflictTokens (..), checkUniqueTokenizing,

    -- * Splitting string on tokens
    TokenizeMap, makeTokenizeMap,
    TokenizeError (..), tokenize
  ) where

import Text.Tokenizer.BlackWhiteSet (BlackWhiteSet(..))
import Text.Tokenizer.Types (Count (..), Token(..), Repeatable(..), getBWS)
import Text.Tokenizer.Uniqueness (ConflictTokens (..), checkUniqueTokenizing)
import Text.Tokenizer.Split
  (TokenizeMap (..), makeTokenizeMap, tokenize, TokenizeError (..))
