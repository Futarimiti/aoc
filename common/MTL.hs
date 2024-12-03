-- | MonadXXX classes re-exports
module MTL
  ( module Control.Monad.Trans.Class
  , module Control.Monad.Trans.Maybe
  , module Control.Monad.IO.Class
  , module Control.Monad.Fix
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Monad.Reader
  , module Control.Monad.RWS
  , module Control.Monad.Except
  , module Control.Monad.Catch
  , exit
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Catch

-- | Early exit
exit :: MonadError e m => e -> m a
exit = throwError
