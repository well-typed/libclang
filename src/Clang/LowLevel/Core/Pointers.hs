module Clang.LowLevel.Core.Pointers (
    CXFile(..)
  , CXPrintingPolicy(..)
  ) where

import Foreign

import Clang.Internal.Results

{-------------------------------------------------------------------------------
  CXFile
-------------------------------------------------------------------------------}

-- | A particular source file that is part of a translation unit.
--
-- NOTE: Equality on 'CXFile' is /pointer/ equality.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#gacfcea9c1239c916597e2e5b3e109215a>
newtype CXFile = CXFile (Ptr ())
  deriving stock (Show, Eq)
  deriving newtype (Storable, IsNullPtr)

{-------------------------------------------------------------------------------
  CXPrintingPolicy
-------------------------------------------------------------------------------}

newtype CXPrintingPolicy = CXPrintingPolicy (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable)
