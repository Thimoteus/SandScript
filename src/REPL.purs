module SandScript.REPL where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Coercible (coerce)

import Data.Array (intersect, replicate)
import Data.Maybe (Maybe, maybe)
import Data.String (indexOf, drop, toCharArray, length)
import Data.Tuple (Tuple(..), fst)
import Data.StrMap (StrMap, toList)
import Data.Foldable (maximumBy, intercalate)
import Data.List (filter)

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

{-- repl :: forall e. Env -> Interface -> Aff (ReplEff e) Unit --}
{-- repl env int = do --}
{--   prompt int --}
{--   input <- readLine int --}
{--   case match input of --}
{--        Command "" -> loop env $ pure unit --}
{--        Command x -> do --}
{--          result <- runComputation env x --}
{--          case result of --}
{--               Left err -> loop env $ print err --}
{--               Right (Tuple env' wff) -> loop env' $ print wff --}
{--        Directive (Show Environment) -> loop env $ log $ pprintEnv env --}
{--        Directive Help -> loop env $ log help --}
{--        Directive Reset -> loop primitiveFuncs (pure unit) --}
{--        Directive (Load m) -> do --}
{--          result <- attempt $ loadEnv m --}
{--          case result of --}
{--               Left err -> loop env $ log $ "Unable to load module: " <> message err --}
{--               Right env' -> loop (env `union` env') $ log $ "module " <> m <> " loaded" --}
{--        Directive (UnknownDirective d) -> loop env $ log $ "Unknown directive: " <> d --}
{--        Directive Quit -> close int --}
{--   where --}
{--     loop s a = do --}
{--       a --}
{--       repl s int --}

matchDirectives :: String -> SSCiDirective
matchDirectives s
  | s ⊆ "help" = Help
  | s ⊆ "quit" = Quit
  | s ⊆ "reset" = Reset
  | s ⊆ "show environment" = Show Environment
  | "load " ⊆ s = Load $ drop 5 s
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
