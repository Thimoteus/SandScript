module SandScript.Parser where

import Data.Identity
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Tuple

import Control.Alt
import Control.Alternative
import Control.Lazy

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import SandScript.Util
import SandScript.Types

type SParser a = Parser String a

many1 :: forall a. Parser String a -> Parser String (Array a)
many1 par = do
  x <- par
  xs <- many par
  return (x:xs)

symbol :: SParser String
symbol = oneOf $ toChars "!#$%&|*+-/:<=>?@^_~"

digit :: SParser String
digit = oneOf $ toChars "0123456789"

letter :: SParser String
letter = oneOf $ toChars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseString :: SParser LispVal
parseString = do
  string "\""
  x <- many (noneOf ["\""] <|> string "\\\"")
  string "\""
  return $ String (unChars x)

parseAtom :: SParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = unChars $ first:rest
  return $ case atom of
                "#t" -> Bool true
                "#f" -> Bool false
                _ -> Atom atom

parseNumber :: SParser LispVal
parseNumber = do
  number <- many1 digit
  return $ Number (str2Num $ unChars number)

parseList :: SParser LispVal -> SParser LispVal
parseList pars = List <$> pars `sepBy` whiteSpace

parseDottedList :: SParser LispVal -> SParser LispVal
parseDottedList pars = do
  head <- pars `endBy` whiteSpace
  tail <- string "." >> whiteSpace >> pars
  return $ DottedList head tail

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

readExpr :: String -> LispVal
readExpr input = case runParser input parseExpr of
                      Left (ParseError err) -> String $ "No match: " ++ err.message
                      Right val -> val
