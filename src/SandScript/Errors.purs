module SandScript.Errors where

import Prelude
import Data.Either
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans
import SandScript.Types

--trapError :: forall e m. (MonadError e m, Show e, Applicative m) => m String -> m String
trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action ((return <<< show) :: LispError -> ThrowsError String)

trapError' :: forall r. EffThrowsError r String -> EffThrowsError r String
trapError' action = catchError action ((return <<< show) :: LispError -> EffThrowsError r String)

extractValue :: forall a. ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: forall r a. ThrowsError a -> EffThrowsError r a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runEffThrows :: forall r.  EffThrowsError r String -> (REff r) String
runEffThrows action = runErrorT (trapError' action) >>= return <<< extractValue

errorT :: forall e m a b. (Functor m) => (e -> b) -> (a -> b) -> ErrorT e m a -> m b
errorT onErr onSucc errT = either onErr onSucc <$> runErrorT errT


{--

runErrorT :: ErrorT e m a -> m (Either e a)
trapError :: m String -> m String
extractValue :: ThrowsError a -> a
ThrowsError a = Either LispError a

action :: EffThrowsError r String :: ErrorT LispError (REff r) String

catchError :: MonadError e m => m a -> (e -> m a) -> m a
--}
