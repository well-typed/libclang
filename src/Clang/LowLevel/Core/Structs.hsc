{-# LANGUAGE RecordWildCards #-}

module Clang.LowLevel.Core.Structs (
    CXCursor_
  , CXSourceLocation_
  , CXSourceRange_
  , CXString_
  , CXToken_
  , CXType_
  , CXUnsavedFile(..)
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

#include <clang-c/Index.h>

-- | <https://clang.llvm.org/doxygen/structCXCursor.html>
data {-# CType "CXCursor" #-} CXCursor_

-- | <https://clang.llvm.org/doxygen/structCXSourceLocation.html>
data {-# CType "CXSourceLocation" #-} CXSourceLocation_

-- | <https://clang.llvm.org/doxygen/structCXSourceRange.html>
data {-# CType "CXSourceRange" #-} CXSourceRange_

-- | <https://clang.llvm.org/doxygen/structCXString.html>
data {-# CType "CXString" #-} CXString_

-- | <https://clang.llvm.org/doxygen/structCXToken.html>
data {-# CType "CXToken" #-} CXToken_

-- | <https://clang.llvm.org/doxygen/structCXType.html>
data {-# CType "CXType" #-} CXType_

-- | Provides the contents of a file that has not yet been saved to disk.
--
-- Each 'CXUnsavedFile' instance provides the name of a file on the system along
-- with the current contents of that file that have not yet been saved to disk.
--
-- <https://clang.llvm.org/doxygen/structCXUnsavedFile.html>
data {-# CType "struct CXUnsavedFile" #-} CXUnsavedFile = CXUnsavedFile {
      cxUnsavedFileFilename :: CString
    , cxUnsavedFileContents :: CString
    , cxUnsavedFileLength   :: CULong
    }

instance Storable CXUnsavedFile where
  sizeOf    _ = #size      struct CXUnsavedFile
  alignment _ = #alignment struct CXUnsavedFile

  peek ptr = do
    cxUnsavedFileFilename <- (#peek struct CXUnsavedFile, Filename) ptr
    cxUnsavedFileContents <- (#peek struct CXUnsavedFile, Contents) ptr
    cxUnsavedFileLength   <- (#peek struct CXUnsavedFile, Length)   ptr
    return CXUnsavedFile{..}

  poke ptr CXUnsavedFile{..} = do
    (#poke struct CXUnsavedFile, Filename) ptr cxUnsavedFileFilename
    (#poke struct CXUnsavedFile, Contents) ptr cxUnsavedFileContents
    (#poke struct CXUnsavedFile, Length)   ptr cxUnsavedFileLength
