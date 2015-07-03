module SandScript.Errors where

import Control.Monad.Error
import Control.Monad.Error.Class
import SandScript.Types

--trapError :: forall e m a. (MonadError e m, Show a) => m String -> m String
--trapError action = catchError action (return <<< show)

retShow :: forall e m. (MonadError e m, Show e) => e -> m String
retShow = return <<< show

--return . show ::: e -> m String
