module Data.Text.Tokenizer.Uniqueness (
    Alt (..),
    Rem (..),
    MergeRes (..), mergeReps, mergedList, remList, rem1, rem2,
    Suff (..), Div (..), initDiv, stepDiv,
    ConflictTokens (..), checkUniqueTokenizing
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (guard, when)
import Data.Type.Bool (type (||), type (&&))
import Data.Bifunctor (Bifunctor(..))
import qualified Data.Set as S
import Data.Coerce (coerce)

import qualified Data.Text.Tokenizer.BlackWhiteSet as BWS
import Data.Text.Tokenizer.Types
  (getBWS, RToken(..), TokId(..), Repeatable(..), castReps, Token (..), makeRToken)

-- | Type synonym for list monad used as a collection of alternatives
newtype Alt a = Alt [a]
  deriving (
      Eq, Ord, Show,
      Functor, Applicative, Monad, Alternative,
      Foldable, Traversable
    )

data Rem r r' c
  -- | First list reminder. May be empty if there is no rem
  = Rem1 [Repeatable r c]
  -- | Second list reminder. Always is nonempty
  | Rem2 [Repeatable r' c]

data MergeRes r r' c = MergeRes
  { merged :: [Repeatable (r && r') c],
    mergeRem   :: Rem r r' c
  }

-- type Tautology :: Bool -> Bool -> Bool -> Constraint
-- type Tautology r r' r'' = (r'' ~ (r || r'), (r && r') ~ 'True => r'' ~ 'True)

mergedList :: (r'' ~ (r || r'), (r && r') ~ 'True => r'' ~ 'True) =>
  MergeRes r r' c -> [Repeatable (r || r') c]
mergedList MergeRes{merged, mergeRem} = castReps merged <> case mergeRem of
  Rem1 xs -> castReps xs
  Rem2 ys -> castReps ys

remList :: MergeRes r r' c -> [Repeatable (r || r') c]
remList MergeRes{mergeRem} = case mergeRem of
  Rem1 res -> castReps res
  Rem2 res -> castReps res

rem1 :: MergeRes r r' c -> [Repeatable r c]
rem1 (MergeRes _ (Rem1 xs)) = xs
rem1 _ = []

rem2 :: MergeRes r r' c -> [Repeatable r' c]
rem2 (MergeRes _ (Rem2 ys)) = ys
rem2 _ = []

mergeReps :: (Ord c) =>
  [Repeatable r c] -> [Repeatable r' c] -> Alt (MergeRes r r' c)
mergeReps xs ys = case (xs, ys) of
  ([], []) -> pure MergeRes {merged = [], mergeRem = Rem1 []}
  (xs, []) -> pure MergeRes {merged = [], mergeRem = Rem1 xs}
  ([], ys) -> pure MergeRes {merged = [], mergeRem = Rem2 ys}
  (x : xs', y : ys') -> do
    let bws = BWS.bwsIntersection (getBWS x) (getBWS y)
    guard $ not $ BWS.isEmpty bws
    case (x, y) of
      (One _, One _) -> do
        res@MergeRes{merged} <- mergeReps xs' ys'
        pure $ res{merged = One bws : merged}
      (One _, Some _) -> do
        res@MergeRes{merged} <- mergeReps xs' ys <|> mergeReps xs' ys'
        pure $ res{merged = One bws : merged}
      (Some _, One _) -> do
        res@MergeRes{merged} <- mergeReps xs ys' <|> mergeReps xs' ys'
        pure $ res{merged = One bws : merged}
      (Some _, Some _) -> do
        res@MergeRes{merged} <-
          mergeReps xs' ys <|> mergeReps xs ys' <|> mergeReps xs' ys'
        pure $ res{merged = Some bws : merged}

-- | Dangling suffix
data Suff c = Suff
  { -- | Symbols behind suffix. Note that only @maxBehind@ symbols are preserved
    srbeh   :: [Repeatable 'True c],
    -- | Symbols from suffix' body
    scur    :: [Repeatable 'True c],
    -- | Symbols ahead suffix
    sahead  :: [Repeatable 'False c]
  }
  deriving (Eq, Ord, Show)

{- | Result of division.

  It looks like

  >       rtoks       |       lastTok
  > --------|---------|-----------------------|~~~~~
  >     rprefToks        |
  > -----|-----|---------|
  > suff (remained part):
  >               behind |     current        | ahead
  >               -------|====================|~~~~~
-}
data Div c = Div
  { -- | Tokens in main sequence, except last one
    rtoks :: [(TokId, Int)],
    -- | Last token in main sequence
    lastTok :: (TokId, Int),
    -- | Tokens in alter sequence
    rprefToks :: [(TokId, Int)],
    -- | Processed symbols
    processed :: [Repeatable 'True c],
    -- | Remained suffix
    suff :: Suff c
  }
  deriving (Eq, Ord, Show)

initDiv :: RToken c -> Div c
initDiv RToken{tokId, body, ahead} =
  Div {
    rtoks = [],
    lastTok = (tokId, 0),
    rprefToks = [],
    suff = Suff {srbeh = [], scur = body, sahead = ahead},
    processed = []
  }

{- 1) Current token is smaller then available part:
  old:    srbeh         scur               sahead
      -------------|=====================|~~~~~~~~~~~~
  cur:   rbehind       body         ahead
      -------------|===========|~~~~~~~~~~~~~~~~~~~~~~
  new:           srbeh'           scur'     sahead'
      -------------------------|=========|~~~~~~~~~~~~

   2) Current token is bigger then available part:
  old:    srbeh        scur                 sahead
      -------------|=====================|~~~~~~~~~~~~~~~~~~~~~~
  cur:   rbehind       body                             ahead
      -------------|===============================|~~~~~~~~~~~~
  new:             srbeh'                   scur'    sahead'
      -----------------------------------|=========|~~~~~~~~~~~~
-}
stepDiv :: (Ord c, Show c) => Int -> Div c -> RToken c -> Alt (Div c)
stepDiv
  maxBehind
  Div{rtoks, rprefToks, lastTok, suff = Suff{srbeh, scur, sahead}, processed}
  RToken{tokId, rbehind, body, ahead} = do
    rbeh <- mergedList <$> mergeReps srbeh rbehind
    cur_body <- mergeReps scur body
    let proc' = processed <> merged cur_body
        srbeh' = take maxBehind $ reverse (merged cur_body) <> rbeh
        len = length (merged cur_body)
    case mergeRem cur_body of
      --      scur      |  sahead
      -- body   |     ahead
      -- srbeh' | scur' |  sahead'
      Rem1 scurRem -> do
        (scur', sahead') <- do
          tmp <- mergeReps scurRem ahead
          let scur' = castReps (merged tmp) <> rem1 tmp
          sahead' <- mergedList <$> mergeReps sahead (rem2 tmp)
          pure (scur', sahead')
        pure Div {
            rtoks,
            rprefToks = (tokId, len) : rprefToks,
            lastTok = second (+ len) lastTok,
            suff = Suff { srbeh = srbeh', scur = scur', sahead = sahead' },
            processed = proc'
          }
      -- scur   |     sahead
      --      body      |  ahead
      -- srbeh' | scur' |  sahead'
      Rem2 bodyRem -> do
        (scur', sahead') <- do
          tmp <- mergeReps sahead bodyRem
          let scur' = castReps (merged tmp) <> rem2 tmp
          sahead' <- mergedList <$> mergeReps (rem1 tmp) ahead
          pure (scur', sahead')
        pure Div {
            rtoks = rprefToks,
            rprefToks = second (+ len) lastTok : rtoks,
            lastTok = (tokId, len),
            suff = Suff {srbeh = srbeh', scur = scur', sahead = sahead'},
            processed = proc'
          }

data ConflictTokens k c = ConflictTokens {
    tokList1, tokList2 :: [(k, [Repeatable 'True c])]
  } deriving (Show, Eq, Ord)

-- | Check if every list composed from the set of tokens can be uniquely decomposed into tokens
checkUniqueTokenizing :: forall k c. (Ord c, Show c) =>
  [Token k c] -> Either (ConflictTokens k c) ()
checkUniqueTokenizing toks = mapM_ (h S.empty)
    [res | p <- allRToks,
           p' <- allRToks,
           p /= p',
           res <- stepDiv maxBehind (initDiv p') p
    ]
  where
    allRToks = Alt $ zipWith makeRToken [0 ..] toks
    maxBehind = maximum $ (\Token {behind} -> length behind) <$> toks
    h :: S.Set (Suff c) -> Div c -> Either (ConflictTokens k c) ()
    h olds curDiv@Div{rprefToks, processed, lastTok, rtoks, suff = suff@Suff{scur}} = do
      when (null scur) $
        Left ConflictTokens {
          tokList1 = hh (reverse rprefToks) processed,
          tokList2 = hh (reverse (lastTok : rtoks)) processed
        }
      mapM_ (h $ S.insert suff olds)
        [ nextDiv | tok <- allRToks,
                    nextDiv@Div{suff = nextSuff} <- stepDiv maxBehind curDiv tok,
                    nextSuff `S.notMember` olds
        ]
    hh :: [(TokId, Int)] -> [Repeatable 'True c] -> [(k, [Repeatable 'True c])]
    hh [] _ = []
    hh ((tokId, len) : xs') bwss = (name, bws) : hh xs' bwss'
      where
        (bws, bwss') = splitAt len bwss
        Token{name} = toks !! coerce tokId
