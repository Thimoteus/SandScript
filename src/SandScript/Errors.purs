module SandScript.Errors where

import Prelude
import Data.Either
import Control.Monad.Error
import Control.Monad.Error.Class
import SandScript.Types

--trapError :: forall e m. (MonadError e m, Show e, Applicative m) => m String -> m String
trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action ((return <<< show) :: LispError -> ThrowsError String)

extractValue :: forall a. ThrowsError a -> a
extractValue (Right val) = val
