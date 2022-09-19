{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Text.Tokenizer
import qualified Text.Tokenizer.BlackWhiteSet as BWS
import Text.Tokenizer.Uniqueness (MergeRes (..), mergeReps, remList)

import Data.Void (Void)
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(..))

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Control.Applicative (Alternative(..))
import Test.Hspec (hspec, describe, it, shouldBe, HasCallStack, Spec)
import Text.Tokenizer.Types (Alt (..))
import Text.Tokenizer.Split (TokenizeError (NoWayTokenize, TwoWaysTokenize))

type Parser = MP.Parsec Void String

parens :: Char -> Char -> Parser a -> Parser a
parens b e = MP.between (MP.char b) (MP.char e)

group :: Char -> Char -> Parser a -> Parser [a]
group b e p = parens b e (many p) <|> (:[]) <$> p

pBWSet :: Parser (BlackWhiteSet Char)
pBWSet = BWS.WhiteSet . S.fromList <$> group '{' '}' MP.letterChar
  <|> MP.char '!' *> (BWS.BlackSet . S.fromList <$> group '{' '}' MP.letterChar)

pRepeatable :: Parser (Repeatable Char)
pRepeatable = do
  symb <- pBWSet
  cnt <- Some <$ MP.char '*' <|> pure One
  pure $ Repeatable cnt symb

pToken :: k -> Parser (Token k Char)
pToken name = do
  behind <- MP.option [] (MP.char '?' *> group '<' '>' pBWSet)
  body <- some pRepeatable
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

instance {-# OVERLAPS #-} Show (Repeatable Char) where
  show (Repeatable One bws) = show bws
  show (Repeatable Some bws) = show bws ++ "*"

instance {-# OVERLAPS #-} Show [Repeatable Char] where
  show = concatMap show

instance {-# OVERLAPS #-} Show k => Show (ConflictTokens k Char) where
  show (ConflictTokens xs ys) = unwords ["Conflicts:", show xs, show ys]

toTuple :: MergeRes c -> ([Repeatable c], [Repeatable c])
toTuple tmp = (merged tmp, remList tmp)

testMerge :: String -> String -> Alt (String, String)
testMerge xsStr ysStr = bimap show show . toTuple <$>
  mergeReps (parse (many pRepeatable) xsStr) (parse (many pRepeatable) ysStr)

showStr :: String -> String
showStr s = if null s then "<empty>" else s

itTestMerge :: HasCallStack => String -> String -> [] (String, String) -> Spec
itTestMerge xsStr ysStr res =
  it (unwords [showStr xsStr, "vs", showStr ysStr]) $
    testMerge xsStr ysStr `shouldBe` Alt res

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

itTestTokenize :: HasCallStack => [String] -> String ->
  Either (TokenizeError [Char] Char) [(String, String)] -> Spec
itTestTokenize tokStrs str res =
  it (show tokStrs <> " vs " <> showStr str) $
    tokenize tokMap str `shouldBe` res
  where
    tokMap = makeTokenizeMap $ (\s -> parse (pToken s) s) <$> tokStrs

itTokOk :: HasCallStack => [String] -> String -> [(String, String)] -> Spec
itTokOk tokStrs str res =
  itTestTokenize tokStrs str (Right res)

itTokNoWay :: [String] -> String -> Int -> [([Char], [Char])] -> Spec
itTokNoWay tokStrs str pos toked =
  itTestTokenize tokStrs str (Left $ NoWayTokenize pos toked)

itTokTwoWays :: [String] -> String -> Int -> [(String, String)] -> [(String, String)]-> Spec
itTokTwoWays tokStrs str pos toked1 toked2 =
  itTestTokenize tokStrs str (Left $ TwoWaysTokenize pos toked1 toked2)

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
    itTestFail ["a*"]
      [("a*", "a*a*"), ("a*", "a*")] [("a*", "a*"), ("a*", "a*a*")]
  describe "tokenizing" $ do
    let a = ("a", "a")
        b = ("b", "b")
    itTokOk [] "" []
    itTokOk ["a"] "" []
    itTokOk ["a"] "a" [a]
    itTokOk ["a", "b"] "ab" [a, b]
    itTokOk ["a"] "aa" [a, a]
    itTokOk ["a?a"] "aa" [("a?a", "a"), ("a?a", "a")]
    itTokOk ["a*?!a"] "aaa" [("a*?!a", "aaa")]

    itTokNoWay ["a?!a"] "aa" 0 []
    itTokNoWay ["a?b"] "ab" 1 [("a?b", "a")]

    itTokTwoWays ["a*"] "aa" 0 [("a*", "a"), ("a*", "a")] [("a*", "aa")]
