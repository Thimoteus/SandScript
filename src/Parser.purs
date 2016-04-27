module SandScript.Parser where

import Prelude

import SandScript.AST (WFF(..))
import SandScript.Errors (ThrowsError, LangError(..))

import Data.Either (Either(..))
import Data.List as L
import Control.Coercible (coerce)
import Data.String (contains)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, try, sepBy, (<?>), sepEndBy)
import Text.Parsing.Parser.String (char, whiteSpace, noneOf, satisfy, string)

import Control.Lazy (fix)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Error.Class (throwError)

infixr 5 L.Cons as :

foreign import str2int :: String -> Int

type P a = Parser String a

many1 :: forall a. P a -> P (L.List a)
many1 p = do
  x <- p
  xs <- L.many p
  pure (x : xs)

anyOf :: String -> P Char
anyOf s = satisfy \ c -> contains (coerce c) s

anyLetter :: P Char
anyLetter = anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

anyDigit :: P Char
anyDigit = anyOf "0123456789"

symbol :: P Char
symbol = anyOf "!#$%&|*+-/:<=>?@^_~"

parseString :: P WFF
parseString =
  String <<< coerce <$> between (char '"') (char '"') (L.many $ noneOf ['"'])

parseAtom :: P WFF
parseAtom = do
  first <- anyLetter <|> symbol
  rest <- L.many $ anyLetter <|> anyDigit <|> symbol
  let atom = coerce $ first : rest
  pure case atom of
            "True" -> Bool true
            "False" -> Bool false
            _ -> Atom atom

parseInteger :: P WFF
parseInteger = Integer <$> do
  xs <- many1 anyDigit
  pure $ str2int $ coerce xs

parseList :: P WFF -> P WFF
parseList p = map List $ sepBy p whiteSpace

parseDotList :: P WFF -> P WFF
parseDotList p = do
  head <- sepEndBy p whiteSpace
  char '.' *> whiteSpace
  tail <- p
  pure $ DotList head tail

parseQuoted :: P WFF -> P WFF
parseQuoted p = do
  string "'"
  x <- p
  pure $ List $ Atom "quote" : x : L.Nil

parseExpr :: P WFF
parseExpr = fix \ p -> try parseAtom
                    <|> try parseString
                    <|> try parseInteger
                    <|> parseQuoted p
                    <|> between (char '(') (char ')') (parseList p)
                    <?> "Malformed input"

read :: String -> ThrowsError WFF
read input = case runParser input parseExpr of
                  Right v -> pure v
                  Left err -> throwError $ ParseErr err
