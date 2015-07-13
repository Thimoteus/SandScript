module SandScript.Eval.EffPrimitives where

import Prelude

import Data.Tuple
import Data.Array

import SandScript.Types

{--
effPrimitives :: forall r. Array (Tuple String (Array LispVal) -> EffThrowsError r LispVal)
effPrimitives = [ "apply" & applyProc
                , "open-input-file" &
                --}
