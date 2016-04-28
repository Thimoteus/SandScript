module SandScript.REPL where

import Prelude

import SandScript.AST (WFF)
import SandScript.Errors (ThrowsError)
import SandScript.Env (Env, State, liftState, runState)
import SandScript.Parser (read)
import SandScript.Eval (eval)

import Control.Monad.State.Class (get)

import Data.Tuple (Tuple(..))
import Data.String (indexOf, drop, toCharArray)
import Data.Maybe (Maybe(..))
import Data.Array (intersect)

data Command = Directive SSCiDirective
             | Command String

data SSCiDirective = Help
                   | Quit
                   | UnknownDirective String

match :: String -> Command
match s | satisfies (eq 0) (indexOf ":" s) = Directive $ matchDirectives $ drop 1 s
match s = Command s

satisfies :: forall a. Eq a => (a -> Boolean) -> Maybe a -> Boolean
satisfies p (Just x) = p x
satisfies _ _ = false

matchDirectives :: String -> SSCiDirective
matchDirectives s
  | s `isSubstring` "quit" = Quit
  | s `isSubstring` "help" = Help
matchDirectives "?" = Help
matchDirectives s = UnknownDirective s

isSubstring :: String -> String -> Boolean
isSubstring sub sup =
  let subA = toCharArray sub
      supA = toCharArray sup
   in intersect subA supA == subA

readEvalString :: forall m. Monad m => String -> State m (Tuple Env WFF)
readEvalString s = do
  wff <- liftState (read s) >>= eval
  env <- get
  pure (Tuple env wff)

runComputation :: forall m. Monad m => Env -> String -> m (ThrowsError (Tuple Env WFF))
runComputation env s = runState env $ readEvalString s

help :: String
help = """
SSCi -- A REPL for SandScript

Commands:

:h, :?  -- prints this message
:q      -- quit SSCi
<x>     -- evaluate <x> as a SandScript expression
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
