module SandScript.JS where

import Prelude

import Data.List as List
import Data.Array as Array
import Data.String (fromCharArray, replace)
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.Maybe (Maybe(..))

infixr 5 List.Cons as :

type Name = String

data JSExpr = JSInt Int
            | JSNumber Number
            | JSBool Boolean
            | JSString String
            | JSSymbol Name
            | JSReturn JSExpr
            | JSLambda (List.List Name) JSExpr
            | JSBinop JSBinop JSExpr JSExpr
            | JSAssign Name JSExpr
            | JSFunCall JSExpr (List.List JSExpr)
            | JSIf JSExpr JSExpr (Maybe JSExpr)
            | JSArray (Array JSExpr)
            | JSList (List.List JSExpr)
            | JSThrow String

data JSBinop = Add | Sub | Mul | Div

indent :: Int -> String -> String
indent tabs e = (fromCharArray $ Array.replicate tabs ' ') <> e

unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"

generateJSOp :: JSBinop -> String
generateJSOp = case _ of
  Add -> " + "
  Sub -> " - "
  Mul -> "*"
  Div -> "/"

generateJS :: Boolean -> Int -> JSExpr -> String
generateJS doindent tabs = case _ of
  JSInt i -> show i
  JSBool b -> show b
  JSNumber n -> show n
  JSSymbol name -> escape name
  JSString s -> show s
  JSLambda vars expr -> (if doindent then indent tabs else id) $ unlines
    [ "function (" <> intercalate ", " vars <> ") {"
    , indent (tabs + 1) $ generateJS false (tabs + 1) expr ] <> indent tabs "}"
  JSBinop op e1 e2 -> "(" <> generateJS false tabs e1 <> " " <> generateJSOp op <> " " <> generateJS false tabs e2 <> ")"
  JSAssign var expr -> "var " <> escape var <> " = " <> generateJS false tabs expr <> ";"
  JSFunCall f exprs -> "(" <> generateJS false tabs f <> ")(" <> intercalate ", " (generateJS false tabs <$> exprs) <> ")"
  JSReturn expr -> (if doindent then indent tabs else id) $ "return " <> generateJS false tabs expr <> ";"
  JSIf cond tr (Just fl) -> (if doindent then indent tabs else id) $
    unlines [ "if (" <> generateJS false tabs cond <> ") {"
            , indent (tabs + 1) $ generateJS false (tabs + 1) tr
            , indent tabs "} else {"
            , indent (tabs + 1) $ generateJS false (tabs + 1) fl
            , indent tabs "}" ]
  JSIf cond tr _ -> (if doindent then indent tabs else id) $
    unlines [ indent tabs "if (" <> generateJS false tabs cond <> ") {"
            , indent (tabs + 1) $ generateJS false (tabs + 1) tr
            , indent tabs "}" ]
  JSArray xs -> "[" <> intercalate ", " (generateJS false tabs <$> xs) <> "]"
  JSList xs -> generateList doindent tabs xs
  JSThrow s -> "throw(new Error(" <> s <> "))"

generateList :: Boolean -> Int -> List.List JSExpr -> String
generateList doindent tabs = case _ of
  (x : xs) -> unlines [ "{val: " <> generateJS doindent tabs x <> ", "
                      , "next:" <> generateList doindent tabs xs <> "}"
                      ]
  _ -> "{}"

escape :: String -> String
escape = until eq singleReplace
  where
  singleReplace = foldl compose id
    [ replace "-" "_"
    , replace "?" "$QUEST"
    , replace "#" "$HASH"
    , replace "%" "$PERCENT"
    , replace ":" "$COLON"
    , replace "<" "$LESSTHAN"
    , replace "=" "$GREATERTHAN"
    , replace "@" "$AT"
    ]

--"!#$%&|*+-/:<=>?@^_~"
until :: forall a. (a -> a -> Boolean) -> (a -> a) -> a -> a
until f g x | f x (g x) = g x
until f g x = until f g (g x)
