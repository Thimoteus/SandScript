module SandScript.Util where

import Prelude

import Data.List.Unsafe (head, tail)
import Data.List hiding (head, tail)
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Maybe
import Data.Tuple

import qualified Data.String as S

import Control.Alternative
import Text.Parsing.Parser

toChars :: String -> List String
toChars = toList <<< S.split ""

unChars :: Array String -> String
unChars = foldr (++) ""

unwordsList :: forall a. (Show a) => List a -> String
unwordsList = unwords <<< map show

unwordsArray :: forall a. (Show a) => Array a -> String
unwordsArray = unwordsArr <<< map show

foldl1 :: forall a. (a -> a -> a) -> List a -> a
foldl1 f xs = foldl f (head xs) (tail xs)

readNum :: String -> Maybe Int
readNum s | s == show (str2num s) = Just $ str2num s
          | otherwise = Nothing

(&) :: forall a b. a -> b -> Tuple a b
(&) = Tuple
infixr 7 &

absInt :: Int -> Int
absInt n
  | n < 0 = -n
  | otherwise = n

showList :: forall a. (Show a) => List a -> String
showList Nil = "()"
showList lst = "(" ++ go lst ++ ")"
  where
  go Nil = ""
  go (Cons x Nil) = show x
  go (Cons x xs) = show x ++ ", " ++ go xs


foreign import str2num :: String -> Int

foreign import unwordsArr :: Array String -> String

unwords :: List String -> String
unwords = unwordsArr <<< fromList
