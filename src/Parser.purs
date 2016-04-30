module SandScript.Parser where

import Prelude
import Data.List as L
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Coercible (coerce)
import Control.Lazy (fix)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.String (contains)
import SandScript.AST (WFF(..), ThrowsError, LangError(..))
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead, endBy, between, choice, (<?>), sepBy, optional, many1Till, try)
import Text.Parsing.Parser.String (char, whiteSpace, noneOf, satisfy, string, anyChar, eof)

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

escaped :: P Char
escaped = char '\\' *> anyOf "\\\"\n\r\t"

parseString :: P WFF
parseString =
  String <<< coerce <$> between (char '"') (char '"') (L.many $ noneOf ['"', '\\'] <|> escaped)

skipComment :: P Unit
skipComment = do
  char ';'
  many1Till anyChar $ char '\n'
  optional eof
  pure unit

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
parseInteger =
  lookAhead anyChar >>= case _ of
                             '-' -> do
                               char '-'
                               digits <- many1 anyDigit
                               pure $ Integer $ str2int $ coerce $ '-' : digits
                             _ -> do
                               digits <- many1 anyDigit
                               pure $ Integer $ str2int $ coerce digits

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
                   , skipComment *> pure (String "")
                   , between (char '(') (char ')') (parseList p) ]
                   <?> "Malformed input"

readOrThrow :: forall a. P a -> String -> ThrowsError a
readOrThrow pwff input = case runParser input pwff of
                              Right v -> pure v
                              Left err -> throwError $ ParseErr err

read :: String -> ThrowsError WFF
read = readOrThrow parseExpr

readFile :: String -> ThrowsError (L.List WFF)
readFile = readOrThrow $ parseExpr `endBy` whiteSpace
