module CodeGen.JS.JS where

import Prelude

import Data.List as List
import Data.Array as Array
import Data.String (fromCharArray, replace)
import Data.Foldable (class Foldable, intercalate, foldl)

infixr 5 List.Cons as :

type Name = String

data JSExpr = JSInt Int
            | JSNumber Number
            | JSBool Boolean
            | JSString String
            | JSSymbol Name
            | JSReturn JSExpr
            | JSLambda (List.List Name) JSExpr
            | JSBinop JSBinop (List.List JSExpr)
            | JSUnop JSUnop JSExpr
            | JSAssign Name JSExpr
            | JSFunCall JSExpr (List.List JSExpr)
            | JSIf JSExpr JSExpr JSExpr
            | JSArray (Array JSExpr)
            | FFI String
            | Export (Array String)

newtype JSBinop = Binop String

newtype JSUnop = Unop String

indent :: Int -> String -> String
indent tabs e = (fromCharArray $ Array.replicate tabs ' ') <> e

unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"

generateJSBinop :: JSBinop -> String
generateJSBinop (Binop s) = s

generateJSUnop :: JSUnop -> String
generateJSUnop (Unop s) = s

generateJS :: Boolean -> Int -> JSExpr -> String
generateJS doindent tabs = case _ of
  JSInt i -> show i
  JSBool b -> show b
  JSNumber n -> show n
  JSSymbol name -> escape name
  JSString s -> show s
  JSLambda vars expr -> (if doindent then indent tabs else id) $ unlines
    [ "function (" <> intercalate ", " vars <> ") {"
    , indent (tabs + 1) $ generateJS false (tabs + 1) expr
    , indent tabs "}" ]
  JSBinop op cs -> "(" <> intercalate (generateJSBinop op) (generateJS false tabs <$> cs) <> ")"
  JSUnop op e -> generateJSUnop op <> "(" <> generateJS false tabs e <> ")"
  JSAssign var expr -> "var " <> escape var <> " = " <> generateJS false tabs expr <> ";"
  JSFunCall f exprs -> "(" <> generateJS false tabs f <> ")(" <> intercalate ", " (generateJS false tabs <$> exprs) <> ")"
  JSReturn expr -> (if doindent then indent tabs else id) $ "return " <> generateJS false tabs expr <> ";"
  JSIf cond tr fl -> (if doindent then indent tabs else id) $
    unlines [ "if (" <> generateJS false tabs cond <> ") {"
            , indent (tabs + 1) $ generateJS false (tabs + 1) tr
            , indent tabs "} else {"
            , indent (tabs + 1) $ generateJS false (tabs + 1) fl
            , indent tabs "}" ]
  JSArray xs -> "[" <> intercalate ", " (generateJS false tabs <$> xs) <> "]"
  FFI s -> s
  Export xs ->
    let firstln = "module.exports = {"
        lastln = "}"
        midlns = intercalate ",\n" $ map (indent 2 <<< (\ x -> show x <> ": " <> escape x)) xs
     in unlines [firstln, midlns, lastln]

--"!#$%&|*+-/:<=>?@^_~"
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
    , replace ">" "$GREATERTHAN"
    , replace "@" "$AT"
    , replace "+" "$PLUS"
    ]

until :: forall a. (a -> a -> Boolean) -> (a -> a) -> a -> a
until f g x | f x (g x) = g x
until f g x = until f g (g x)
