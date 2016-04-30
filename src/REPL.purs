module SandScript.REPL where

import Prelude

import SandScript.Env (Env)
import SandScript.Eval (runComputation, primitiveFuncs)

import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Aff.Console (print, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Coercible (coerce)

import Data.Array (intersect, replicate)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe, maybe)
import Data.String (indexOf, drop, toCharArray, length)
import Data.Tuple (Tuple(..), fst)
import Data.StrMap (StrMap, toList)
import Data.Foldable (maximumBy, intercalate)
import Data.List (filter)

import Node.ReadLine as RL

data Command = Directive SSCiDirective
             | Command String

data SSCiDirective = Help
                   | Quit
                   | Reset
                   | Show SSCiShow
                   | UnknownDirective String

data SSCiShow = Environment

type ReplEff e = ( console :: CONSOLE, readline :: RL.READLINE, err :: EXCEPTION | e )

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
  log sandScript
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
       Directive (UnknownDirective d) -> loop env $ log $ "Unknown directive: " <> d
       Directive Quit -> close int
  where
    loop s a = do
      a
      repl s int

match :: String -> Command
match s | satisfies (eq 0) (indexOf ":" s) = Directive $ matchDirectives $ drop 1 s
match s = Command s

satisfies :: forall a. Eq a => (a -> Boolean) -> Maybe a -> Boolean
satisfies = maybe false

matchDirectives :: String -> SSCiDirective
matchDirectives s
  | s ⊆ "help" = Help
  | s ⊆ "quit" = Quit
  | s ⊆ "reset" = Reset
  | s ⊆ "show environment" = Show Environment
matchDirectives "?" = Help
matchDirectives s = UnknownDirective s

isSubstring :: String -> String -> Boolean
isSubstring sub sup =
  let subA = toCharArray sub
      supA = toCharArray sup
   in intersect subA supA == subA

infix 0 isSubstring as ⊆

pprintEnv :: forall a. Show a => StrMap a -> String
pprintEnv xs =
  let list = toList xs
      longestKeyM = maximumBy (\ (Tuple k _) (Tuple k' _) -> compare (length k) (length k')) list
      longestKey = maybe "" fst longestKeyM
      l = length longestKey + 5
      stringify (Tuple k v) = k <> duplicate (l - length k) ' ' <> show v
   in intercalate "\n" $ map stringify $ filter (\(Tuple _ v) -> not ("<primitive>" ⊆ show v)) list

duplicate :: Int -> Char -> String
duplicate n c = coerce $ replicate n c

help :: String
help = """
SSCi -- A REPL for SandScript

Commands:

:h, :?      -- prints this message
:show env   -- list the current identifiers and definitions
:reset      -- unbind all user-defined atoms
:q          -- quit SSCi
<x>         -- evaluate <x> as a SandScript expression
"""

sandScript :: String
sandScript = """
  ()  _,         _|   ()  _   ,_  o    _|_
  /\ / |  /|/|  / |   /\ /   /  | | |/\_|
 /(_)\/|_/ | |_/\/|_//(_)\__/   |/|/|_/ |_/
                                   (|
:? to see available commands
"""
--  __             __              
-- / _| _    _  ||/ _| _ _ () _ || 
-- \_ \/o\ |/ \/o|\_ \///_|||/o\| ]
-- |__/\_,]L_n|\_||__/\\L| L||_/L| 
--                           L|    
--  ____                         __  ____                               __      
-- /\  _`\                      /\ \/\  _`\                  __        /\ \__   
-- \ \,\L\_\     __      ___    \_\ \ \,\L\_\    ___   _ __ /\_\  _____\ \ ,_\  
--  \/_\__ \   /'__`\  /' _ `\  /'_` \/_\__ \   /'___\/\`'__\/\ \/\ '__`\ \ \/  
--    /\ \L\ \/\ \L\.\_/\ \/\ \/\ \L\ \/\ \L\ \/\ \__/\ \ \/ \ \ \ \ \L\ \ \ \_ 
--    \ `\____\ \__/.\_\ \_\ \_\ \___,_\ `\____\ \____\\ \_\  \ \_\ \ ,__/\ \__\
--     \/_____/\/__/\/_/\/_/\/_/\/__,_ /\/_____/\/____/ \/_/   \/_/\ \ \/  \/__/
--                                                                  \ \_\       
--                                                                   \/_/       
                                           
--   ()  _,         _|   ()  _   ,_  o    _|_ 
--   /\ / |  /|/|  / |   /\ /   /  | | |/\_|  
--  /(_)\/|_/ | |_/\/|_//(_)\__/   |/|/|_/ |_/
--                                    (|      
