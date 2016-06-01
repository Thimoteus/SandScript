module Parser.Common where

import Control.Coercible (coerce)

import Data.String (contains)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (satisfy)

anyOf :: String -> Parser String Char
anyOf s = satisfy \ c -> contains (coerce c) s
