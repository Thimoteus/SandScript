module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Tuple (snd, fst)
import Data.List (List(..), fromFoldable, reverse)
import Data.String (split)
import Data.Breadcrumbs (Breadcrumbs, insert, moveDown, moveUp, mkBreadcrumb, extract, filter)

import Pux (start, fromSimple, renderToDOM)
import Pux.Html (Html, text, form)
import Pux.Html.Attributes (name, type_, value, id_, className, htmlFor, autoFocus)
import Pux.Html.Elements (p, div, input, label)
import Pux.Html.Events (FormEvent, onChange, onSubmit, onKeyDown)

import Partial.Unsafe (unsafePartial)

import Signal.Channel (CHANNEL)

import SandScript.Env (Env)
import SandScript.Eval (primitiveFuncs)
import SandScript.REPL (repl, asciiart)

infixr 6 Cons as :

main :: Eff (channel :: CHANNEL, err :: EXCEPTION) Unit
main = do
  app <- start { initialState: initState
               , update: fromSimple update
               , view: view
               , inputs: [] }
  renderToDOM "#app" app.html

initState :: State
initState = { lines: sandScript
            , prompt: ""
            , history: mkBreadcrumb ""
            , env: primitiveFuncs }

data Action = Write FormEvent
            | MoveUp
            | MoveDown
            | Enter

type State = { lines :: List String
             , prompt :: String
             , history :: Breadcrumbs String
             , env :: Env }

update :: Action -> State -> State
update (Write ev) s = s { prompt = ev.target.value }
update MoveUp s =
  let hist = moveUp s.history
   in s { history = hist, prompt = extract hist }
update MoveDown s =
  let hist = moveDown s.history
   in s { history = hist, prompt = extract hist }
update Enter s =
  let env = s.env
      inp = s.prompt
      result = repl env inp
      newEnv = fst result
      newLn = snd result
   in { lines: splitLines newLn <> ("> " <> inp) : s.lines
      , prompt: ""
      , history: insert "" $ filter (_ /= "") $ insert inp s.history
      , env: newEnv }

view :: State -> Html Action
view state = div
  [ id_ "main" ]
  [ form
    [ name "simplerepl"
    , id_ "simplerepl"
    , onSubmit $ const Enter ]
    [ div
      []
      (strToP <$> getStateText state)
    , label
      [ htmlFor "input" ]
      [ text ">" ]
    , input
      [ type_ "text"
      , value state.prompt
      , autoFocus true
      , onKeyDown handleKeyDown
      , onChange Write
      , id_ "input"
      ]
      []
    ]
  ]
    where
      handleKeyDown ke = unsafePartial case ke.keyCode of
                                            38 -> MoveUp
                                            40 -> MoveDown

getStateText :: State -> Array String
getStateText = revListToArray <<< _.lines

strToP :: forall a. String -> Html a
strToP s = p [ className "repln" ] [ text s ]

revListToArray :: forall a. List a -> Array a
revListToArray = revListToArray' [] where
  revListToArray' acc Nil = acc
  revListToArray' acc (x : xs) = revListToArray' ([x] <> acc) xs

sandScript :: List String
sandScript = splitLines asciiart

splitLines :: String -> List String
splitLines = reverse <<< fromFoldable <<< split "\n"
