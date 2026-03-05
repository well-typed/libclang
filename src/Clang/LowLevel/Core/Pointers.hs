module Clang.LowLevel.Core.Pointers (
    CXIndex(..)
  , CXTranslationUnit(..)
  , CXTargetInfo(..)
  , CXFile(..)
  , CXPrintingPolicy(..)
  , CXEvalResult(..)
  , CXDiagnostic(..)
  , CXDiagnosticSet(..)
  ) where

import Foreign

import Clang.Internal.Results

{-------------------------------------------------------------------------------
  CXIndex
-------------------------------------------------------------------------------}

-- | An "index" that consists of a set of translation units that would typically
-- be linked together into an executable or library.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gae039c2574bfd75774ca7a9a3e55910cb>
newtype CXIndex = CXIndex (Ptr ())
  deriving stock (Show)

{-------------------------------------------------------------------------------
  CXTranslationUnit
-------------------------------------------------------------------------------}

-- | A single translation unit, which resides in an index.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gacdb7815736ca709ce9a5e1ec2b7e16ac>
newtype CXTranslationUnit = CXTranslationUnit (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable, IsNullPtr)

{-------------------------------------------------------------------------------
  CXTargetInfo
-------------------------------------------------------------------------------}

-- | An opaque type representing target information for a given translation
-- unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#ga6b47552ab8c5d81387070a9b197cd3e2>
newtype {-# CType "CXTargetInfo" #-} CXTargetInfo = CXTargetInfo (Ptr ())
  deriving stock (Show)

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

{-------------------------------------------------------------------------------
  CXEvalResult
-------------------------------------------------------------------------------}

-- | An opaque type representing an evaluation result
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__MISC.html#gaa9270afc68877e1f3b20ce5b343191bc>
newtype {-# CType "CXEvalResult" #-} CXEvalResult = CXEvalResult (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable)

{-------------------------------------------------------------------------------
  CXDiagnostic
-------------------------------------------------------------------------------}

-- | A single diagnostic, containing the diagnostic's severity, location, text,
-- source ranges, and fix-it hints.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga44bb8aba7c40590ad25d1763c4fbff7f>
newtype {-# CType "CXDiagnostic" #-} CXDiagnostic = CXDiagnostic (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable)

{-------------------------------------------------------------------------------
  CXDiagnosticSet
-------------------------------------------------------------------------------}

-- | A group of CXDiagnostics.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__DIAG.html#ga38dfc0ae45b55bf7fd577eed9148e244>
newtype CXDiagnosticSet = CXDiagnosticSet (Ptr ())
