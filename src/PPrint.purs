module PPrint where

import Prelude

import Control.Monad.Transformerless.State (State, execState, get, modify)
import Data.Foldable (intercalate)
import Data.String.Yarn (leftpadBy)

parens :: String -> String
parens m = "(" <> m <> ")"

type PrinterState =
  { indent :: Int
  , log:: Array String
  , line :: String
  , uuid :: Int
  }
type Printer a = State PrinterState a

initState :: PrinterState
initState = {indent: 0, log: [], line: "", uuid: 0}

mappend :: String -> Printer Unit
mappend s = modify \ st -> st {line = st.line <> s}

showAndAppend :: forall a. Show a => a -> Printer Unit
showAndAppend = mappend <<< show

commitLine :: Printer Unit
commitLine = do
  {line, indent} <- get
  modify \ st -> st {log = st.log <> [leftpadBy indent line], line = ""}

indent :: Printer Unit
indent = modify \ st -> st {indent = st.indent + 2}

unindent :: Printer Unit
unindent = modify \ st -> st {indent = st.indent - 2}

fresh :: Printer String
fresh = do
  {uuid} <- get
  modify \ st -> st {uuid = st.uuid + 1}
  pure $ "$" <> show uuid

runPrinter :: Printer Unit -> String
runPrinter p =
  case execState p initState of
    {indent, line, log} ->
      intercalate "\n" $ log <> [leftpadBy indent line]

execPrinter :: forall a. PrinterState -> Printer a -> PrinterState
execPrinter s p = execState p s
