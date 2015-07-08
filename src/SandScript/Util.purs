module SandScript.Util where

import Prelude

import Data.Array.Unsafe (head, tail)
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Maybe

import qualified Data.String as S

import Control.Alternative
import Text.Parsing.Parser

(>>) :: forall m a b. (Monad m) => m a -> m b -> m b
(>>) f g = f >>= \ _ -> g

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

foreign import str2num :: String -> Int

foreign import unwords :: Array String -> String
