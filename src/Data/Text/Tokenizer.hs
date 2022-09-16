module Data.Text.Tokenizer (
    BlackWhiteSet (..), bwsSingle, bwsIntersection,
    Repeatable (..), getBWS, castRep, castReps,
    TokId, Token (..),

    ConflictTokens (..), checkUniqueTokenizing
  ) where

import Data.Text.Tokenizer.BlackWhiteSet
  (BlackWhiteSet(..), bwsSingle, bwsIntersection)
import Data.Text.Tokenizer.Types
    (TokId, Token(..), Repeatable(..), getBWS, castRep, castReps)
import Data.Text.Tokenizer.Uniqueness
  (ConflictTokens (..), checkUniqueTokenizing)
