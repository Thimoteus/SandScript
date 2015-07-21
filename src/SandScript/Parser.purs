module SandScript.Parser where

import Prelude
import Math (pow)

import Data.List (fromList, many, (:), List(..))
import Data.Identity
import Data.Maybe
import Data.String
import Data.Char (toString)
import Data.Either
import Data.Foldable
import Data.Tuple
import Data.Int (toNumber, even)
import qualified Data.Array as A
import qualified Data.Char as C

import Control.Alt
import Control.Alternative
import Control.Lazy
import Control.Apply ((*>))
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

many1 :: forall a. Parser String a -> Parser String (List a)
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
  return $ String (foldMap C.toString x)

parseAtom :: SParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = foldMap C.toString $ first:rest
  return $ case atom of
                "true" -> Bool true
                "false" -> Bool false
                _ -> Atom atom

parseInt :: SParser LispVal
parseInt = do
  minus <- many $ char '~'
  number <- many1 digit
  if even $ Data.List.length minus
     then return <<< Int <<< str2num $ foldMap C.toString number
     else return <<< Int <<< negate <<< str2num $ foldMap C.toString number

parseFloat :: SParser LispVal
parseFloat = do
  minus <- many $ char '~'
  integral <- foldMap C.toString <$> many1 digit
  char '.'
  fractional <- foldMap C.toString <$> many1 digit
  if even $ Data.List.length minus
     then return <<< Float $ toFloat integral fractional
     else return <<< Float $ toNegFloat integral fractional
    where
    toNegFloat :: String -> String -> Number
    toNegFloat intg frct = (toNumber <<< negate $ str2num intg) - (decimalize $ str2num frct)

toFloat :: String -> String -> Number
toFloat intg frct = (toNumber $ str2num intg) + (decimalize $ str2num frct)
decimalize :: Int -> Number
decimalize n = (toNumber n) / pow 10.0 (toNumber $ length $ show n)

-- many :: f a -> f (List a)
-- char :: Char -> SParser Char
parseFrac :: SParser LispVal
parseFrac = do
  minus <- many $ char '~'
  numer <- foldMap C.toString <$> many1 digit
  char '/'
  denom <- foldMap C.toString <$> many1 digit
  if even $ Data.List.length minus
     then return <<< Frac $ str2num numer & str2num denom
     else return <<< Frac $ (negate $ str2num numer) & str2num denom

parseComplex :: SParser LispVal
parseComplex = do
  realMinus <- many $ char '~'
  realInteg <- foldMap C.toString <$> many1 digit
  char '.'
  realFrac <- foldMap C.toString <$> many1 digit
  char '+'
  imaginaryMinus <- many $ char '~'
  imaginaryInteg <- foldMap C.toString <$> many1 digit
  char '.'
  imaginaryFrac <- foldMap C.toString <$> many1 digit
  char 'i'
  let rMinus = if even $ Data.List.length realMinus then 1.0 else -1.0
      iMinus = if even $ Data.List.length imaginaryMinus then 1.0 else -1.0
      real = rMinus * (toNumber (str2num realInteg) + (decimalize $ str2num realFrac))
      imaginary = iMinus * (toNumber (str2num imaginaryInteg) + (decimalize $ str2num imaginaryFrac))
  return $ Complex { real: real, imaginary: imaginary }

parseComment :: SParser (Data.List.List Char)
parseComment = do
  char ';'
  many1Till anyChar (char '\n')

skipComment :: SParser Unit
skipComment = do
  parseComment
  optional eof
  return unit

parseList :: SParser LispVal -> SParser LispVal
parseList pars = List <$> sepBy pars whiteSpace

parseDottedList :: SParser LispVal -> SParser LispVal
parseDottedList pars = do
  init <- pars `endBy` whiteSpace
  last <- char '.' *> whiteSpace *> pars
  return $ DottedList init last

parseQuoted :: SParser LispVal -> SParser LispVal
parseQuoted pars = do
  string "'"
  x <- pars
  return $ List $ (Atom "quote") : x : Nil

parseVector :: SParser LispVal -> SParser LispVal
parseVector pars = do
  x <- pars `sepBy` (oneOf [' ', ','])
  return $ Vector (fromList x)

parseExpr :: SParser LispVal
parseExpr = fix $ \ p -> (parseString
                     <|> (try parseComplex <|> try parseFloat <|> try parseFrac <|> try parseInt)
                     <|> parseAtom
                     <|> (parseQuoted p)
                     <|> (do
                         skipComment
                         return $ String "")
                     <|> (do
                         string "["
                         x <- parseVector p
                         string "]"
                         return x)
                     <|> (do
                         string "("
                         x <- try (parseList p) <|> (parseDottedList p)
                         string ")"
                         return x))

readOrThrow :: forall a. SParser a -> String -> ThrowsError a
readOrThrow parser input = case runParser input parser of
                                Left err -> throwError $ Parserr err
                                Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr whiteSpace)
