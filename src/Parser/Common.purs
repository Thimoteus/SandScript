module SandScript.Parser.Common where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Coercible (coerce)

import Data.List (List, many, (:))
import Data.String (contains)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (skipMany, try, skipMany1) as P
import Text.Parsing.Parser.String (satisfy, char, string) as P

type P a = Parser String a

whiteSpace :: P Unit
whiteSpace = P.skipMany (simpleSpace <|> oneLineComment)

simpleSpace :: P Unit
simpleSpace = P.skipMany1 $ P.satisfy \ c -> c == ' ' || c == '\n' || c == '\r' || c == '\t'

oneLineComment :: P Unit
oneLineComment = do
  P.try $ P.char ';'
  P.skipMany $ P.satisfy (_ /= '\n')

lexeme :: forall a. P a -> P a
lexeme p = p <* whiteSpace

symbol :: String -> P String
symbol = lexeme <<< P.string

anyOf :: String -> P Char
anyOf s = P.satisfy \ c -> contains (coerce c) s

many1 :: forall a. P a -> P (List a)
many1 p = do
  x <- p
  xs <- many p
  pure (x : xs)

anyLetter :: P Char
anyLetter = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

anyDigit :: P Char
anyDigit = anyOf "0123456789"

int :: P Int
int = do
  f <- sign
  n <- many1 anyDigit
  pure $ f $ str2int $ coerce n

sign :: P (Int -> Int)
sign = (P.char '-' *> pure negate) <|> (P.char '+' *> pure id) <|> pure id

str2int :: String -> Int
str2int = fromMaybe 0 <<< fromString
