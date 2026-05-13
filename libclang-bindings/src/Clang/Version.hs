-- | @libclang@ version API
module Clang.Version (
    -- * Definition
    ClangVersion(..)
  , parseClangVersion
    -- * Versions in use
  , compileTimeClangVersionString
  , compileTimeClangVersion
  , runtimeClangVersionString
  , runtimeClangVersion
    -- * Version requirements
  , isCompatibleClangVersion
  , Requires
  , requireClangVersion
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

import Version_libclang_bindings qualified

{-------------------------------------------------------------------------------
  Versions in use
-------------------------------------------------------------------------------}

-- | Version of @libclang@ linked at compile-time (string)
compileTimeClangVersionString :: Text
compileTimeClangVersionString =
    Version_libclang_bindings.clangVersionCompileTime

-- | Version of @libclang@ linked at compile-time
compileTimeClangVersion :: ClangVersion
compileTimeClangVersion = parseClangVersion compileTimeClangVersionString

-- | Version of @libclang@ loaded at runtime (string)
runtimeClangVersionString :: Text
runtimeClangVersionString = unsafePerformIO $ preallocate_ wrap_getClangVersion
{-# NOINLINE runtimeClangVersionString #-}

-- | Version of @libclang@ loaded at runtime
runtimeClangVersion :: ClangVersion
runtimeClangVersion = parseClangVersion runtimeClangVersionString

{-------------------------------------------------------------------------------
  Version requirements
-------------------------------------------------------------------------------}

-- | Check for compatibility of two Clang versions
--
-- Two Clang versions are compatible if they have the same major and minor
-- versions.
isCompatibleClangVersion :: ClangVersion -> ClangVersion -> Bool
isCompatibleClangVersion l r =
    fromMaybe False $ Applicative.liftA2 (==) (proj l) (proj r)
  where
    proj :: ClangVersion -> Maybe (Int, Int)
    proj = \case
      ClangVersion (major, minor, _patch) -> Just (major, minor)
      ClangVersionUnknown{}               -> Nothing

-- | Version requirement
--
-- @Requires a@ means that version @a@ or later is required.  For example,
-- @Requires (17, 0, 0)@ means that LLVM/Clang 17 or later is required.
newtype Requires a = Requires a
  deriving stock (Show)

-- | Check that the version of @libclang@ loaded at runtime is greather than or
-- equal to the specified version
--
-- 'CallFailed' is thrown if the current @libclang@ version is not greater than
-- or equal to the specified version, or if it is unknown.
requireClangVersion :: (MonadIO m, HasCallStack) => (Int, Int, Int) -> m ()
requireClangVersion v =
    case runtimeClangVersion of
      ClangVersion version | version >= v ->
        return ()
      _otherwise ->
        callFailedShow (Requires v)
