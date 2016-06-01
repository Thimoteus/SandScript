module REPL where

import Prelude
import Env (Env)
import Eval (runComputation, runComputations, primitiveFuncs)

import Control.Coercible (coerce)
import Control.Monad.Aff (Aff, makeAff, attempt)
import Control.Monad.Aff.Console (print, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error, message)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Error.Class (throwError)

import Data.Array (intersect, replicate, (!!))
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe, maybe, fromMaybe')
import Data.String (indexOf, drop, toCharArray, length)
import Data.Tuple (Tuple(..), fst)
import Data.StrMap (StrMap, toList, union)
import Data.Foldable (maximumBy, intercalate)
import Data.List (filter)

import Node.ReadLine as RL
import Node.FS (FS)
import Node.FS.Aff (readTextFile, exists)
import Node.Encoding (Encoding(UTF8))

data Command = Directive SSCiDirective
             | Command String

data SSCiDirective = Help
                   | Quit
                   | Reset
                   | Load String
                   | Show SSCiShow
                   | UnknownDirective String

data SSCiShow = Environment

type ReplEff e = ( console :: CONSOLE, readline :: RL.READLINE, err :: EXCEPTION, fs :: FS, random :: RANDOM | e )

type Interface = RL.Interface

createInterface :: forall e. Aff (ReplEff e) Interface
createInterface = liftEff (RL.createConsoleInterface RL.noCompletion)

prompt :: forall e. Interface -> Aff (ReplEff e) Unit
prompt i = liftEff $ RL.prompt i

setPrompt :: forall e. String -> Int -> Interface -> Aff (ReplEff e) Unit
setPrompt s n i = liftEff $ RL.setPrompt s n i

close :: forall e. Interface -> Aff (ReplEff e) Unit
close i = liftEff $ RL.close i

readLine :: forall e. Interface -> Aff (ReplEff e) String
readLine i = makeAff $ const $ RL.setLineHandler i

runRepl :: forall e. Aff (ReplEff e) Unit
runRepl = do
  interface <- createInterface
  i <- liftEff $ randomInt 0 2
  log $ fromMaybe' (\_ -> defaulttext) $ asciitexts !! i
  log ":? to see available commands"
  setPrompt ">> " 2 interface
  prompt interface
  repl primitiveFuncs interface

repl :: forall e. Env -> Interface -> Aff (ReplEff e) Unit
repl env int = do
  prompt int
  input <- readLine int
  case match input of
       Command "" -> loop env $ pure unit
       Command x -> do
         result <- runComputation env x
         case result of
              Left err -> loop env $ print err
              Right (Tuple env' wff) -> loop env' $ print wff
       Directive (Show Environment) -> loop env $ log $ pprintEnv env
       Directive Help -> loop env $ log help
       Directive Reset -> loop primitiveFuncs (pure unit)
       Directive (Load m) -> do
         result <- attempt $ loadEnv m
         case result of
              Left err -> loop env $ log $ "Unable to load module: " <> message err
              Right env' -> loop (env `union` env') $ log $ "module " <> m <> " loaded"
       Directive (UnknownDirective d) -> loop env $ log $ "Unknown directive: " <> d
       Directive Quit -> close int
  where
    loop s a = do
      a
      repl s int

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

readFile :: forall e. String -> Aff (ReplEff e) String
readFile file =
  exists file >>= if _
                     then readTextFile UTF8 file
                     else throwError $ error $ file <> " does not exist"

loadEnv :: forall e. String -> Aff (ReplEff e) Env
loadEnv file = do
  input <- readFile file
  result <- runComputations primitiveFuncs input
  case result of
       Left err -> throwError $ error $ show err
       Right (Tuple env _) -> pure env

pprintEnv :: forall a. Show a => StrMap a -> String
pprintEnv xs =
  let list = toList xs
      longestKeyM = maximumBy (\ (Tuple k _) (Tuple k' _) -> compare (length k) (length k')) list
      longestKey = maybe "" fst longestKeyM
      l = length longestKey + 5
      stringify (Tuple k v) = k <> duplicate (l - length k) ' ' <> show v
   in intercalate "\n" $ map stringify $ filter (\(Tuple _ v) -> not ("<primitive>" `isSubstring` show v)) list

duplicate :: Int -> Char -> String
duplicate n c = coerce $ replicate n c

help :: String
help = """
SSCi -- A REPL for SandScript

Commands:

:h, :?      -- prints this message
:show env   -- list the current identifiers and definitions
:load file  -- load all definitions from a file
:reset      -- unbind all user-defined atoms
:q          -- quit SSCi
<x>         -- evaluate <x> as a SandScript expression
"""

asciitexts :: Array String
asciitexts = [defaulttext, ascii1, ascii2]

defaulttext :: String
defaulttext = """
 ()  _,         _|   ()  _   ,_  o    _|_
 /\ / |  /|/|  / |   /\ /   /  | | |/\_|
/(_)\/|_/ | |_/\/|_//(_)\__/   |/|/|_/ |_/
                                  (|
"""
ascii1 :: String
ascii1 = """
 __             __
/ _| _    _  ||/ _| _ _ () _ ||
\_ \/o\ |/ \/o|\_ \///_|||/o\| ]
|__/\_,]L_n|\_||__/\\L| L||_/L|
                          L|
"""
ascii2 :: String
ascii2 = """
 ____                         __  ____                               __
/\  _`\                      /\ \/\  _`\                  __        /\ \__
\ \,\L\_\     __      ___    \_\ \ \,\L\_\    ___   _ __ /\_\  _____\ \ ,_\
 \/_\__ \   /'__`\  /' _ `\  /'_` \/_\__ \   /'___\/\`'__\/\ \/\ '__`\ \ \/
   /\ \L\ \/\ \L\.\_/\ \/\ \/\ \L\ \/\ \L\ \/\ \__/\ \ \/ \ \ \ \ \L\ \ \ \_
   \ `\____\ \__/.\_\ \_\ \_\ \___,_\ `\____\ \____\\ \_\  \ \_\ \ ,__/\ \__\
    \/_____/\/__/\/_/\/_/\/_/\/__,_ /\/_____/\/____/ \/_/   \/_/\ \ \/  \/__/
                                                                 \ \_\
                                                                  \/_/
"""
