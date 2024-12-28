-- | MonadXXX classes re-exports
module MTL
  ( module Control.Monad.Trans
  , module Control.Monad.Trans.Maybe
  , module Control.Monad.Fix
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Monad.Reader
  , module Control.Monad.RWS
  , module Control.Monad.Except
  , module Control.Monad.Catch
  , module Control.Monad.Logger
  , module Control.Monad.Accum
  , module Control.Monad.Trans.Accum
  , module Control.Monad.Cont
  , exit
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Accum
import Control.Monad.Trans.Accum hiding (accum, look, looks, add, liftCallCC, liftListen, liftCatch, liftPass)
import Control.Monad.Cont

-- | Early exit
exit :: MonadError e m => e -> m a
exit = throwError
