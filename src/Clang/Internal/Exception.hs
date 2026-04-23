{-# LANGUAGE CPP #-}
module Clang.Internal.Exception
  ( throwIO
  , try
  , RunInIO
  , handleUnliftUsing
  ) where

import Control.Exception (SomeException)
import Control.Exception qualified as Base
import Control.Monad.IO.Class (MonadIO (..))

{-------------------------------------------------------------------------------
  Internal: exception handling

  We do not use @handle@ from @unlift@, as it excludes async exceptions, and we
  want it to be up to the exception handler to decide if it wants to deal with
  async exceptions or not.
-------------------------------------------------------------------------------}

-- GHC 9.12 introduces `rethrowIO`, the `WhileHandling` annotation, and `catchNoPropagate`
#if defined(MIN_VERSION_GLASGOW_HASKELL) && MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

throwIO :: MonadIO m => SomeException -> m a
throwIO e = liftIO (Base.rethrowIO (Base.ExceptionWithContext (Base.someExceptionContext e) e))

try :: IO a -> IO (Either SomeException a)
try m = Base.catchNoPropagate (Right <$> m) (\(Base.ExceptionWithContext _ e) -> pure (Left e))

type RunInIO m = forall a. m a -> IO a

handleUnliftUsing :: MonadIO n => RunInIO m -> (SomeException -> m a) -> m a -> n a
handleUnliftUsing runInIO handler action = liftIO $
    Base.catchNoPropagate (runInIO action) (\(Base.ExceptionWithContext _ e) -> runInIO (handler e))

#else

-- GHC 9.10 introduces exception annotations
-- NOTE: in this version, `catch` does not decorate the caught exceptions with `WhileHandling`
-- for this reason, `catch` behaves like `catchNoPropagate` in GHC 9.12 onwards
#if defined(MIN_VERSION_GLASGOW_HASKELL) && MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)

newtype Rethrow = Rethrow SomeException
    deriving (Show)

instance Base.Exception Rethrow where
    fromException = Just . Rethrow
    toException (Rethrow e) = e
    backtraceDesired _ = False

throwIO :: MonadIO m => SomeException -> m a
throwIO e = liftIO (Base.throwIO (Rethrow e))

-- earlier GHC versions do not support exception annotations
#else

throwIO :: MonadIO m => SomeException -> m a
throwIO = liftIO . Base.throwIO

#endif

-- `catch` in GHC 9.10 and prior does not annotate the handler with `WhileHandling`
-- therefore, we simply delegate to functions from `Control.Exception`

try :: IO a -> IO (Either SomeException a)
try = Base.try

type RunInIO m = forall a. m a -> IO a

handleUnliftUsing :: MonadIO n => RunInIO m -> (SomeException -> m a) -> m a -> n a
handleUnliftUsing runInIO handler action
  = liftIO (Base.handle (runInIO . handler) (runInIO action))

#endif
