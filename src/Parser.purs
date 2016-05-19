module SandScript.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Error.Class (throwError)

import Data.Functor (($>))
import Data.Either (Either(..))
import Data.List as L
import Data.Array as A

import SandScript.AST (WFF(..), ThrowsError, LangError(..))
import SandScript.Lexer (token)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (try, choice)

infixr 5 L.Cons as :

type P a = Parser String a

string :: P WFF
string = String <$> token.stringLiteral

atom :: P WFF
atom = Atom <$> token.identifier

bool :: P WFF
bool = token.reserved "True" $> Bool true <|> token.reserved "False" $> Bool false

int :: P WFF
int = Integer <$> token.integer

float :: P WFF
float = Float <$> token.float

list :: P WFF -> P WFF
list p = token.parens (List <$> L.many p)

vector :: P WFF -> P WFF
vector p = token.brackets (Vector <$> A.many p)

quote :: P WFF -> P WFF
quote p = do
  token.reservedOp "'"
  x <- p
  pure $ List $ Atom "quote" : x : L.Nil

expr :: P WFF
expr = fix allExprs
  where
    allExprs p = choice
      [ list p
      , vector p
      , quote p
      , atom
      , try float
      , int
      , string
      , bool
      ]

readOrThrow :: forall a. P a -> String -> ThrowsError a
readOrThrow pwff input = case runParser input pwff of
                              Right v -> pure v
                              Left err -> throwError $ ParseErr err

read :: String -> ThrowsError WFF
read = readOrThrow expr

readFile :: String -> ThrowsError (L.List WFF)
readFile = readOrThrow manyExpr
  where
    manyExpr = do
      token.whiteSpace
      L.many expr
