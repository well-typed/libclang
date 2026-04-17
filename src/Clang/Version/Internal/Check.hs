{-# LANGUAGE TemplateHaskell #-}

-- | Use 'checkUserClangVersion' to process the @CLANG_VERSION@ macro
module Clang.Version.Internal.Check () where

import Clang.Version.Internal (checkUserClangVersion)

$(checkUserClangVersion)
