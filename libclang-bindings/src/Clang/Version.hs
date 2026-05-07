module Clang.Version (
    -- * Definition
    ClangVersion(..)
    -- * Version compatibility
  , isClangVersionCompatible
    -- * Current version
  , clangVersionCompileTime
  , clangVersion
    -- * Version requirements
  , Requires
  , requireClangVersion
    -- * Low-level API
  , clang_getClangVersion
  , parseClangVersion
  ) where

import Control.Applicative qualified as Applicative
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import Clang.Internal.ByValue
import Clang.Internal.CXString ()
import Clang.Internal.Results
import Clang.LowLevel.FFI
import Clang.Version.Internal (ClangVersion (..), parseClangVersion)

import Version_libclang_bindings (clangVersionCompileTime)

{-------------------------------------------------------------------------------
  Version compatibility
-------------------------------------------------------------------------------}

-- | Check for compatibility of two Clang versions
--
-- Two Clang versions are compatible if they have the same major and minor
-- versions.
isClangVersionCompatible :: ClangVersion -> ClangVersion -> Bool
isClangVersionCompatible l r =
    fromMaybe False $ Applicative.liftA2 (==) (proj l) (proj r)
  where
    proj :: ClangVersion -> Maybe (Int, Int)
    proj = \case
      ClangVersion (major, minor, _patch) -> Just (major, minor)
      ClangVersionUnknown{}               -> Nothing

{-------------------------------------------------------------------------------
  Current version
-------------------------------------------------------------------------------}

clangVersion :: ClangVersion
clangVersion = unsafePerformIO $ parseClangVersion <$> clang_getClangVersion

{-------------------------------------------------------------------------------
  Version requirements
-------------------------------------------------------------------------------}

-- | Version requirement
--
-- @Requires a@ means that version @a@ or later is required.  For example,
-- @Requires 'Clang17_or_18_or_19'@ means that Clang 17 or later is required.
newtype Requires a = Requires a
  deriving stock (Show)

-- | Check @clang@ major version
--
-- Throw 'CallFailed' if the current Clang version is not greater than or equal
-- to the specified Clang version, or if the clang version is unknown.
requireClangVersion :: (MonadIO m, HasCallStack) => (Int, Int, Int) -> m ()
requireClangVersion v =
    case clangVersion of
      ClangVersion version | version >= v ->
       return ()
      _otherwise ->
        callFailedShow (Requires v)

{-------------------------------------------------------------------------------
  Low-level
-------------------------------------------------------------------------------}

clang_getClangVersion :: MonadIO m => m Text
clang_getClangVersion = liftIO $ preallocate_ $ wrap_getClangVersion
