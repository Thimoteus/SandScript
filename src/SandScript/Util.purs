module SandScript.Util where

import Prelude

import Data.Array.Unsafe (head, tail, unsafeIndex)
import Data.Array (length, drop)
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Maybe
import Data.Tuple

import qualified Data.String as S

import Control.Alternative
import Text.Parsing.Parser

toChars :: String -> Array String
toChars = S.split ""

unChars :: Array String -> String
unChars = foldr (++) ""

unwordsList :: forall a. (Show a) => Array a -> String
unwordsList = unwords <<< (map show)

foldl1 :: forall a. (a -> a -> a) -> Array a -> a
foldl1 f xs = foldl f (head xs) (tail xs)

readNum :: String -> Maybe Int
readNum s | s == show (str2num s) = Just $ str2num s
          | otherwise = Nothing

uncons2 :: forall a. Array a -> Maybe { first :: a, second :: a, rest :: Array a }
uncons2 xs
  | length xs < 3 = Nothing
  | otherwise = Just { first: xs `unsafeIndex` 0, second: xs `unsafeIndex` 1, rest: drop 2 xs }

(&) :: forall a b. a -> b -> Tuple a b
(&) = Tuple
infixr 0 &

absInt :: Int -> Int
absInt n
  | n < 0 = -n
  | otherwise = n

foreign import str2num :: String -> Int

foreign import unwords :: Array String -> String
