module Lang.Haskell98.Util where

import Prelude

type Id = String

enumId :: Int -> Id
enumId n = "v" <> show n
