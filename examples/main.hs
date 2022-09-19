{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Data.Set as S
import Data.Void (Void)

import Control.Applicative (Alternative(..))

import Text.Tokenizer
import GHC.Stack (HasCallStack)

-- ** Overlapping orphan instances for prettier error messages
instance {-# OVERLAPS #-} Show (BlackWhiteSet Char) where
  show (WhiteSet ws) = case S.toList ws of
    [c] -> [c]
    cs -> "{" ++ cs ++ "}"
  show (BlackSet bs) = "!" ++ show (WhiteSet bs)

instance {-# OVERLAPS #-} Show (Repeatable Char) where
  show (Repeatable One bws) = show bws
  show (Repeatable Some bws) = show bws ++ "*"

instance {-# OVERLAPS #-} Show [Repeatable Char] where
  show = concatMap show

instance {-# OVERLAPS #-} Show (ConflictTokens String Char) where
  show (ConflictTokens xs ys) = unwords ["Conflicts:", show xs, show ys]

-- ** Parsers
type Parser = MP.Parsec Void String

parens :: Char -> Char -> Parser a -> Parser a
parens b e = MP.between (MP.char b) (MP.char e)

group :: Char -> Char -> Parser a -> Parser [a]
group b e p = parens b e (many p) <|> (:[]) <$> p

pBWSet :: Parser (BlackWhiteSet Char)
pBWSet = WhiteSet . S.fromList <$> group '{' '}' MP.letterChar
  <|> MP.char '!' *> (BlackSet . S.fromList <$> group '{' '}' MP.letterChar)

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

-- | Unsafe run token's parser. The name of token will just an input string
--
-- Throws error if 'Left' is returned by 'MP.parse'
parse :: HasCallStack => String -> Token String Char
parse str = case MP.parse (pToken str <* MP.eof) "" str of
  Left peb -> error (MP.errorBundlePretty peb)
  Right res -> res

main :: IO ()
main = putStrLn "Examples will be here"
