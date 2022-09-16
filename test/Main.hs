{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Text.Tokenizer (Token(..), Repeatable(..), BlackWhiteSet, checkUniqueTokenizing)
import qualified Text.Tokenizer.BlackWhiteSet as BWS
import Text.Tokenizer.Uniqueness (MergeRes (..), Alt (..), mergeReps, ConflictTokens (..), remList)

import Data.Void (Void)
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(..))
import Data.Type.Bool (type (||), type (&&))

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Control.Applicative (Alternative(..))
import Test.Hspec (hspec, describe, it, shouldBe, HasCallStack, Spec)

type Parser = MP.Parsec Void String

parens :: Char -> Char -> Parser a -> Parser a
parens b e = MP.between (MP.char b) (MP.char e)

group :: Char -> Char -> Parser a -> Parser [a]
group b e p = parens b e (many p) <|> (:[]) <$> p

pBWSet :: Parser (BlackWhiteSet Char)
pBWSet = BWS.WhiteSet . S.fromList <$> group '{' '}' MP.letterChar
  <|> MP.char '!' *> (BWS.BlackSet . S.fromList <$> group '{' '}' MP.letterChar)

pRepeatable :: Parser (Repeatable 'True Char)
pRepeatable = do
  symb <- pBWSet
  Some symb <$ MP.char '*' <|> pure (One symb)

pToken :: k -> Parser (Token k Char)
pToken name = do
  behind <- MP.option [] (MP.char '?' *> group '<' '>' pBWSet)
  body <- many pRepeatable
  ahead <- MP.option [] (MP.char '?' *> group '<' '>' pBWSet)
  pure Token {..}

parse :: Parser a -> String -> a
parse p str = case MP.parse (p <* MP.eof) "" str of
  Left peb -> error (MP.errorBundlePretty peb)
  Right res -> res

instance {-# OVERLAPS #-} Show (BlackWhiteSet Char) where
  show (BWS.WhiteSet ws) = case S.toList ws of
    [c] -> [c]
    cs -> "{" ++ cs ++ "}"
  show (BWS.BlackSet bs) = "!" ++ show (BWS.WhiteSet bs)

instance {-# OVERLAPS #-} Show (Repeatable r Char) where
  show (One bws) = show bws
  show (Some bws) = show bws ++ "*"

instance {-# OVERLAPS #-} Show [Repeatable r Char] where
  show = concatMap show

instance {-# OVERLAPS #-} Show k => Show (ConflictTokens k Char) where
  show (ConflictTokens xs ys) = unwords ["Conflicts:", show xs, show ys]

toTuple :: MergeRes r r' c -> ([Repeatable (r && r') c], [Repeatable (r || r') c])
toTuple tmp = (merged tmp, remList tmp)

testMerge :: String -> String -> Alt (String, String)
testMerge xsStr ysStr = bimap show show . toTuple <$>
  mergeReps (parse (many pRepeatable) xsStr) (parse (many pRepeatable) ysStr)

itTestMerge :: HasCallStack => String -> String -> [] (String, String) -> Spec
itTestMerge xsStr ysStr res = it (unwords [h xsStr, "vs", h ysStr]) $
  testMerge xsStr ysStr `shouldBe` Alt res
  where h s = if null s then "<empty>" else s

itTestUnique :: HasCallStack => [String] -> Spec
itTestUnique tokStrs = it (show tokStrs) $
  checkUniqueTokenizing ((\s -> parse (pToken s) s) <$> tokStrs) `shouldBe` Right ()

itTestFail :: HasCallStack => [String] -> [(String, String)] -> [(String, String)] -> Spec
itTestFail tokStrs toks1 toks2 = it (show tokStrs) $
  checkUniqueTokenizing ((\s -> parse (pToken s) s) <$> tokStrs)
    `shouldBe` Left ConflictTokens {
        tokList1 = second (parse (many pRepeatable)) <$> toks1,
        tokList2 = second (parse (many pRepeatable)) <$> toks2
      }

main :: IO ()
main = hspec $ do
  describe "mergeReps" $ do
    itTestMerge "" "" [("", "")]
    itTestMerge "" "abc" [("", "abc")]
    itTestMerge "aa*" "a" [("a", "a*")]
    itTestMerge "a*{ab}" "a" [("a", "a*{ab}"), ("a", "{ab}")]
    itTestMerge "a*aa*" "a" [("a", "a*aa*"), ("a", "aa*")]
    itTestMerge "{ab}a" "ba" [("ba", "")]
    itTestMerge "{ab}a" "ab" []
    itTestMerge "{ab}{abc}" "{cb}{adb}" [("b{ab}", "")]
    itTestMerge "{ab}*ba*" "aba" $
      map ("aba", ) ["{ab}*ba*", "ba*", "a*", ""]
    itTestMerge "a*" "{ab}*" [("a*", "{ab}*"), ("a*", "a*"), ("a*", "")]
  describe "unique tokenizing" $ do
    itTestUnique []
    itTestUnique ["a"]
    itTestUnique ["a", "b"]
    itTestUnique ["a", "ab"]
    itTestUnique ["a", "bb"]
    itTestUnique ["a", "ab", "bb"]
    itTestUnique ["a", "{ab}b"]
    itTestUnique ["a", "?<!a>b", "ab"]
    itTestUnique ["a?<!b>", "b", "ab"]
    itTestUnique ["a?<a>", "b", "ab"]
  describe "not unique" $ do
    itTestFail ["a", "a"]
      [("a", "a")] [("a", "a")]
    itTestFail ["a", "b", "ab"]
      [("a", "a"), ("b", "b")] [("ab", "ab")]
    itTestFail ["a", "{ab}"]
      [("a", "a")] [("{ab}", "a")]
    itTestFail ["?<a>a", "b", "ab"]
      [("?<a>a", "a"), ("b", "b")] [("ab", "ab")]
