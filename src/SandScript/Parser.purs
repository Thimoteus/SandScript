module SandScript.Parser where

import Prelude

import Data.List (fromList)
import Data.Identity
import Data.Maybe
import Data.String
import Data.Either
import Data.Foldable
import Data.Tuple
import Data.Array (many, (:))

import Control.Alt
import Control.Alternative
import Control.Lazy
import Control.Monad.Error
import Control.Monad.Error.Class

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import SandScript.Types
import SandScript.Util

type SParser a = Parser String a

many1 :: forall a. Parser String a -> Parser String (Array a)
many1 par = do
  x <- par
  xs <- many par
  return (x:xs)

symbol :: SParser Char
symbol = oneOf $ toCharArray "!#$%&|*+-/:<=>?@^_~"

digit :: SParser Char
digit = oneOf $ toCharArray "0123456789"

letter :: SParser Char
letter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

escapedChars :: SParser Char
escapedChars = do
  char '\\'
  x <- oneOf ['\\', '"', '\n', '\r', '\t']
  return x

parseString :: SParser LispVal
parseString = do
  char '"'
  x <- many (noneOf ['"', '\\'] <|> escapedChars)
  char '"'
  return $ String (fromCharArray x)

parseAtom :: SParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = fromCharArray $ first:rest
  return $ case atom of
                "#t" -> Bool true
                "#f" -> Bool false
                _ -> Atom atom

parseNumber :: SParser LispVal
parseNumber = do
  number <- many1 digit
  return <<< Number <<< str2num $ fromCharArray number

parseList :: SParser LispVal -> SParser LispVal
parseList pars = do
  x <- pars `sepBy` whiteSpace
  return $ List (fromList x)

parseDottedList :: SParser LispVal -> SParser LispVal
parseDottedList pars = do
  head <- pars `endBy` whiteSpace
  tail <- string "." >> whiteSpace >> pars
  return $ DottedList (fromList head) tail

parseQuoted :: SParser LispVal -> SParser LispVal
parseQuoted pars = do
  string "'"
  x <- pars
  return $ List [Atom "quote", x]

parseExpr :: SParser LispVal
parseExpr = fix $ \ p -> (parseString
                     <|> parseAtom
                     <|> parseNumber
                     <|> (parseQuoted p)
                     <|> (do
                         string "("
                         x <- try (parseList p) <|> (parseDottedList p)
                         string ")"
                         return x))

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser input (whiteSpace >> parseExpr) of
                      Left err -> throwError $ Parserr err
                      Right val -> return val
