module Clang.Args (
    -- * Clang arguments
    ClangArgs(..)
  , InvalidClangArgs(..)
  ) where

import Control.Exception (Exception)
import Data.Default (Default (def))
import Data.String (IsString)

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

-- | Command-line arguments passed to @libclang@
--
-- The order of command-line arguments is significant.
--
-- Reference:
--
-- * <https://clang.llvm.org/docs/ClangCommandLineReference.html>
newtype ClangArgs = ClangArgs { unClangArgs :: [String] }
  deriving stock (Show)
  deriving newtype (Eq)

instance Default ClangArgs where
  def = ClangArgs []

-- | Invalid Clang arugments exception
newtype InvalidClangArgs = InvalidClangArgs String
  deriving stock (Show)
  deriving newtype (IsString)
  deriving anyclass (Exception)
