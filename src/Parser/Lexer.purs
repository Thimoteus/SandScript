module Lexer where

import Parser.Common (anyOf)

import Control.Alt ((<|>))

import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser, letter, alphaNum)
import Text.Parsing.Parser.String (oneOf)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: ";"
  , nestedComments: false
  , identStart: letter <|> symbol
  , identLetter: alphaNum <|> symbol
  , opStart: oneOf ['\'']
  , opLetter: oneOf []
  , reservedNames: ["True", "False"]
  , reservedOpNames: ["'"]
  , caseSensitive: true
}
    where
      symbol = anyOf "!#$%&|*+-/:<=>?@^_~"

token :: TokenParser
token = makeTokenParser languageDef
