module Text.Tokenizer.Split where

import Data.Map (Map)

import Text.Tokenizer.Types
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Text.Tokenizer.BlackWhiteSet (BlackWhiteSet(..))
import Data.Bifunctor (Bifunctor(..))
import Control.Monad.State (State, evalState, gets)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldrM)
import Control.Monad (guard)
import Data.Coerce (coerce)
import qualified Text.Tokenizer.BlackWhiteSet as BWS
import Control.Applicative (Alternative(..))
import qualified Data.IntMap as Im

modifyId :: (TokId -> TokId) -> RToken c -> RToken c
modifyId f tok@RToken {tokId} = tok {tokId = f tokId}


data TokenizeMap k c = TokenizeMap {
    tokCount    :: Int,
    charTokMap  :: Map c [RToken c],
    blackToks   :: [RToken c],
    tokNames    :: IntMap k
  } deriving (Show)

instance Ord c => Semigroup (TokenizeMap k c) where
  TokenizeMap tokCount' tokMap' blackToks' tokNames'
    <> TokenizeMap tokCount'' tokMap'' blackToks'' tokNames'' =
      TokenizeMap
        { tokCount = tokCount' + tokCount'',
          charTokMap = M.unionWith (<>) tokMap' tokMap''',
          blackToks = blackToks' <> blackToks''',
          tokNames = tokNames' <> tokNames'''
        }
      where
        tokMap''' = map (modifyId (+ tokCount')) <$> tokMap''
        blackToks''' = map (modifyId (+ tokCount')) blackToks''
        tokNames''' = IM.mapKeysMonotonic (+ tokCount') tokNames''

instance Ord c => Monoid (TokenizeMap k c) where
  mempty = TokenizeMap 0 mempty mempty mempty

singleTokMap :: Ord c => Token k c -> TokenizeMap k c
singleTokMap tok@Token {name, body} =
  TokenizeMap
    { tokCount = 1,
      charTokMap = case bws of
        BlackSet _ -> mempty
        WhiteSet s -> M.fromAscList . map (, [rtok]) . S.toList $ s,
      blackToks = case bws of
        BlackSet _ -> [rtok]
        WhiteSet _ -> mempty,
      -- tokMap = M.fromAscList $ map (,[rtok]) $ toList $ head body,
      tokNames = IM.singleton tokId name
    }
  where
    tokId = 0
    bws = getBWS $ head body
    rtok = makeRToken tokId tok

-- | Insert 'Token' into 'TokenizeMap'
insert :: Ord c => Token k c -> TokenizeMap k c -> TokenizeMap k c
insert tok = (<> singleTokMap tok)

-- | Create auxillary Map for tokenizing. Should be called once for initializing
makeTokenizeMap :: Ord c => [Token k c] -> TokenizeMap k c
makeTokenizeMap = foldr insert mempty

-- | Error during tokenizing
data TokenizeError k c
  = NoWayTokenize
      Int
      -- ^ Position of the first character that can not be tokenized
      [(k, [c])]
      -- ^ Part of string successfully tokenized (the longest of all attempts)
  | TwoWaysTokenize
      Int
      -- ^ Length of uniquely tokenized prefix
      [(k, [c])]
      -- ^ First tokenize way
      [(k, [c])]
      -- ^ Second tokenize way
  deriving (Show, Eq)

mapTokErrKey :: (k -> k') -> TokenizeError k c -> TokenizeError k' c
mapTokErrKey f (NoWayTokenize pos toks) =
  NoWayTokenize pos (map (first f) toks)
mapTokErrKey f (TwoWaysTokenize pos toks toks') =
  TwoWaysTokenize pos (map (first f) toks) (map (first f) toks')

-- | Split list of symbols on tokens.
tokenize :: forall k c. Ord c => TokenizeMap k c -> [c] -> Either (TokenizeError k c) [(k, [c])]
tokenize TokenizeMap {charTokMap, blackToks, tokNames} cs =
    bimap nameTokErr nameTokRes $ flip evalState mempty $ h 0 [] cs
  where
    nameTokErr :: TokenizeError TokId c -> TokenizeError k c
    nameTokErr = mapTokErrKey (tokNames Im.!)
    nameTokRes :: [(TokId, [c])] -> [(k, [c])]
    nameTokRes = map $ first (tokNames IM.!)
    -- input string is split in two parts: (reversed) @prevs@ and @nexts@
    -- @pos == length prevs@
    -- prevs are assumed to be already processed
    -- returns unique possible first token's result at the @pos@ position
    h :: Int -> [c] -> [c] -> State (IntMap (Res c)) (Res c)
    h _ _ [] = pure $ Right []
    h pos prevs nexts@(cur : _) = do
      -- get memorized result
      mres <- gets $ IM.lookup pos
      maybe acceptedToks pure mres
      where
        -- List of all tokens to be considered at current position
        allToks :: [RToken c]
        allToks = blackToks <> fromMaybe [] (M.lookup cur charTokMap)

        acceptedToks :: State (IntMap (Res c)) (Res c)
        acceptedToks =
          foldrM
            ( \(tokId, curs, nexts') res'' -> do
                let curTok = (tokId, curs)
                res' <- addTok curTok <$>
                  h (pos + length curs) (reverse curs <> prevs) nexts'
                pure $ case (res', res'') of
                  (Left TwoWaysTokenize {}, _) -> res'
                  (_, Left TwoWaysTokenize {}) -> res''
                  (Left NoWayTokenize {}, Right _) -> res''
                  (Right _, Left NoWayTokenize {}) -> res'
                  (Left (NoWayTokenize l' _), Left (NoWayTokenize l'' _)) ->
                    if l' > l'' then res' else res''
                  (Right toks', Right toks'') ->
                    Left $ TwoWaysTokenize pos toks' toks''
            )
            ((Left $ NoWayTokenize pos []) :: Res c)
            $ concatMap (coerce $ accepts prevs nexts) allToks

addTok :: (TokId, [c]) -> Res c -> Res c
addTok tok = \case
  Left (NoWayTokenize pos toks) ->
    Left $ NoWayTokenize pos (tok : toks)
  Left (TwoWaysTokenize len toks toks') ->
    Left $ TwoWaysTokenize len (tok : toks) (tok : toks')
  Right rs -> Right $ tok : rs

accepts :: Ord c => [c] -> [c] -> RToken c -> Alt (TokId, [c], [c])
accepts rprevs nexts RToken {tokId, rbehind, body, ahead} = do
  guard $ not . null $ check rbehind rprevs
  (curs, rem) <- check body nexts
  case rem of
    RemRepeatable _ -> empty -- not enough symbols for token's body
    RemString cs' -> do
      guard $ not . null $ check ahead cs'
      pure (tokId, curs, cs')

data Rem c = RemRepeatable [Repeatable c] | RemString [c]
  deriving (Eq, Ord, Show)

check :: Ord c => [Repeatable c] -> [c] -> Alt ([c], Rem c)
check [] cs = pure ([], RemString cs)
check rs [] = pure ([], RemRepeatable rs)
check rs0@(Repeatable cnt bws : rs) (c : cs) = do
  guard $ c `BWS.member` bws
  first (c :) <$> case cnt of
    One -> check rs cs
    Some -> check rs cs <|> check rs0 cs

type Res c = Either (TokenizeError TokId c) [(TokId, [c])]

infixr 9 .:
(.:) :: (b -> c) -> (a -> a' -> b) -> a -> a' -> c
(.:) = fmap . fmap

