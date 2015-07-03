module SandScript.Util where

import Data.Array.Unsafe (head)
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Maybe
import qualified Data.String as S

import Control.Alternative

import Text.Parsing.Parser

toChars :: String -> [String]
toChars = S.split ""

unChars :: [String] -> String
unChars = foldr (++) ""

foreign import str2Num
  """function str2Num(str) {
    return str | 0;
  }""" :: String -> Number

(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \ _ -> g

foreign import unWords
  """function unWords(strs) {
    return strs.join(" ");
  }""" :: [String] -> String

unwordsList :: forall a. (Show a) => [a] -> String
unwordsList = unWords <<< (show <$>)

foldl1 :: forall a. (a -> a -> a) -> Array a -> a
foldl1 f (x:xs) = foldl f x xs
