module SandScript.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Coercible (coerce)
import Control.Lazy (fix)
import Control.Monad.Error.Class (throwError)

import Data.Either (Either(..))
import Data.List as L

import SandScript.AST (WFF(..), ThrowsError, LangError(..))
import SandScript.Parser.Common (whiteSpace, symbol, int, anyDigit, anyLetter, anyOf)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy, between, choice, try, (<?>))
import Text.Parsing.Parser.String (char, string, noneOf)

infixr 5 L.Cons as :

type P a = Parser String a

anyTill :: Char -> P String
anyTill c = do
  xs <- L.many $ noneOf [c]
  char c
  pure $ coerce xs

anySymbol :: P Char
anySymbol = anyOf "!#$%&|*+-/:<=>?@^_~"

escaped :: P Char
escaped = char '\\' *> anyOf "\\\"\n\r\t"

parseString :: P WFF
parseString =
  String <<< coerce <$> between (char '"') (char '"') (L.many $ noneOf ['"', '\\'] <|> escaped)

parseAtom :: P WFF
parseAtom = do
  first <- anyLetter <|> anySymbol
  rest <- L.many $ anyLetter <|> anyDigit <|> anySymbol
  let atom = coerce $ first : rest
  pure case atom of
            "True" -> Bool true
            "False" -> Bool false
            _ -> Atom atom

parseInteger :: P WFF
parseInteger = Integer <$> int

parseList :: P WFF -> P WFF
parseList p = List <$> sepBy p whiteSpace

parseQuoted :: P WFF -> P WFF
parseQuoted p = do
  string "'"
  x <- p
  pure $ List $ Atom "quote" : x : L.Nil

parseExpr :: P WFF
parseExpr = fix \ p ->
            choice [ try parseInteger
                   , parseString
                   , parseAtom
                   , parseQuoted p
                   , between (symbol "(") (symbol ")") (parseList p) ]
                   <?> "well-formed formula"

readOrThrow :: forall a. P a -> String -> ThrowsError a
readOrThrow pwff input = case runParser input pwff of
                              Right v -> pure v
                              Left err -> throwError $ ParseErr err

read :: String -> ThrowsError WFF
read = readOrThrow parseExpr

readFile :: String -> ThrowsError (L.List WFF)
readFile = readOrThrow $ parseExpr `sepBy` whiteSpace
