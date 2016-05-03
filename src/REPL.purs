module SandScript.REPL where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Coercible (coerce)
import Control.Monad.Trampoline (runTrampoline)

import Data.Array (intersect, replicate)
import Data.Maybe (Maybe, maybe)
import Data.String (indexOf, drop, toCharArray, length)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..))
import Data.StrMap (StrMap, toList)
import Data.Foldable (maximumBy, intercalate)
import Data.List (filter)

import SandScript.Env (Env)
import SandScript.Eval (primitiveFuncs, runComputation)

data Command = Directive SSCiDirective
             | Command String

data SSCiDirective = Help
                   | Quit
                   | Reset
                   | Load String
                   | Show SSCiShow
                   | UnknownDirective String

data SSCiShow = Environment

type ReplEff e = ( console :: CONSOLE, err :: EXCEPTION | e )

runOne :: Env -> String -> Tuple Env String
runOne env inp = runTrampoline do
  res <- runComputation env inp
  case res of
       Left err -> pure $ Tuple env $ show err
       Right (Tuple env wff) -> pure $ Tuple env $ show wff

repl :: Env -> String -> Tuple Env String
repl env inp =
  case match inp of
       Command "" -> Tuple env ""
       Command x -> runOne env x
       Directive (Show Environment) -> Tuple env $ pprintEnv env
       Directive Help -> Tuple env $ help
       Directive Reset -> Tuple primitiveFuncs "environment reset"
       Directive (UnknownDirective d) -> Tuple env $ "Unknown directive: " <> d
       Directive (Load m) -> Tuple env "Unsupported in this version"
       Directive Quit -> Tuple env "Unsupported in this version"

matchDirectives :: String -> SSCiDirective
matchDirectives s
  | s `isSubstring` "help" = Help
  | s `isSubstring` "quit" = Quit
  | s `isSubstring` "reset" = Reset
  | s `isSubstring` "show environment" = Show Environment
  | "load " `isSubstring` s = Load $ drop 5 s
matchDirectives "?" = Help
matchDirectives s = UnknownDirective s

match :: String -> Command
match s | satisfies (eq 0) (indexOf ":" s) = Directive $ matchDirectives $ drop 1 s
match s = Command s

satisfies :: forall a. Eq a => (a -> Boolean) -> Maybe a -> Boolean
satisfies = maybe false

isSubstring :: String -> String -> Boolean
isSubstring sub sup =
  let subA = toCharArray sub
      supA = toCharArray sup
   in intersect subA supA == subA

pprintEnv :: forall a. Show a => StrMap a -> String
pprintEnv xs =
  let list = toList xs
      longestKeyM = maximumBy (\ (Tuple k _) (Tuple k' _) -> compare (length k) (length k')) list
      longestKey = maybe "" fst longestKeyM
      l = length longestKey + 5
      stringify (Tuple k v) = k <> duplicate (l - length k) '-' <> show v
   in intercalate "\n" $ map stringify $ filter (\(Tuple _ v) -> not ("<primitive>" `isSubstring` show v)) list

duplicate :: Int -> Char -> String
duplicate n c = coerce $ replicate n c

help :: String
help = """
A Web REPL for SandScript -- github.com/thimoteus/SandScript

Commands:

:h, :? ------- prints this message
:show env ---- list the current identifiers and definitions
:reset ------- unbind all user-defined atoms
:q ----------- quit SSCi
<x> ---------- evaluate <x> as a SandScript expression
"""


asciiart :: String
asciiart = """
:'######:::::'###::::'##::: ##:'########:::'######:::'######::'########::'####:'########::'########:
'##... ##:::'## ##::: ###:: ##: ##.... ##:'##... ##:'##... ##: ##.... ##:. ##:: ##.... ##:... ##..::
.##:::..:::'##:. ##:: ####: ##: ##:::: ##: ##:::..:: ##:::..:: ##:::: ##:: ##:: ##:::: ##:::: ##::::
. ######::'##:::. ##: ## ## ##: ##:::: ##:. ######:: ##::::::: ########::: ##:: ########::::: ##::::
:..... ##: #########: ##. ####: ##:::: ##::..... ##: ##::::::: ##.. ##:::: ##:: ##.....:::::: ##::::
'##::: ##: ##.... ##: ##:. ###: ##:::: ##:'##::: ##: ##::: ##: ##::. ##::: ##:: ##::::::::::: ##::::
. ######:: ##:::: ##: ##::. ##: ########::. ######::. ######:: ##:::. ##:'####: ##::::::::::: ##::::
:......:::..:::::..::..::::..::........::::......::::......:::..:::::..::....::..::::::::::::..:::::
:? to see available commands
github.com/thimoteus/SandScript for source
"""
