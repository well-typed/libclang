{-# LANGUAGE OverloadedStrings #-}

module Clang.CStandard (
    -- * C standard
    CStandard(..)
  , Gnu(..)
  , ClangCStandard(..)
    -- * Querying @libclang@
  , getClangCStandard
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

import Clang.Args (ClangArgs)
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.Internal.Results
import Clang.LowLevel.Core
import Clang.Paths

{-------------------------------------------------------------------------------
  C standard
-------------------------------------------------------------------------------}

-- | C standard
--
-- Reference:
--
-- * "C Support in Clang"
--   <https://clang.llvm.org/c_status.html>
-- * "Differences between various standard modes" in the clang user manual
--   <https://clang.llvm.org/docs/UsersManual.html#differences-between-various-standard-modes>
data CStandard =
    C89
  | C95
  | C99
  | C11
  | C17
  | C23
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Enable GNU extensions?
data Gnu =
    DisableGnu
  | EnableGnu
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Clang C standard
data ClangCStandard = ClangCStandard {
      cStandard :: CStandard
    , gnu       :: Gnu
    }
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  Querying @libclang@
-------------------------------------------------------------------------------}

-- | Get the C standard for the specified 'ClangArgs'
--
-- Reference:
--
-- * <https://clang.llvm.org/docs/UsersManual.html#differences-between-various-standard-modes>
-- * <https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html>
getClangCStandard :: ClangArgs -> IO (Maybe ClangCStandard)
getClangCStandard = fmap (aux =<<) . getClangCStandardBuiltinMacros
  where
    aux :: (Bool, Maybe Integer, Bool) -> Maybe ClangCStandard
    aux (isStdc, mStdcVersion, isGnu)
      | isStdc =
          let gnu'       = if isGnu then EnableGnu else DisableGnu
              valid std' = Just $ ClangCStandard std' gnu'
          in  case mStdcVersion of
                Nothing     -> valid C89
                Just 199409 -> valid C95
                Just 199901 -> valid C99
                Just 201112 -> valid C11
                Just 201710 -> valid C17  -- c17 6~
                Just 202000 -> valid C23  -- c2x 14~17
                Just 202311 -> valid C23  -- c23 18.1.0~
                Just _other -> Nothing
      | otherwise = Nothing

-- | Get the values of builtin macros used to determine the C standard
--
-- This function gets the values for three macros:
--
-- 1. @__STDC__@ ('Bool'), used to detect C89
-- 1. @__STDC_VERSION__@ ('Integer')
-- 2. @linux@ ('Bool'), used to detect GNU extensions
--
-- 'Nothing' is returned if there is an error.
getClangCStandardBuiltinMacros ::
     ClangArgs
  -> IO (Maybe (Bool, Maybe Integer, Bool))
getClangCStandardBuiltinMacros clangArgs =
    HighLevel.withUnsavedFile filename contents $ \unsavedFile ->
      HighLevel.withIndex DontDisplayDiagnostics $ \index ->
        HighLevel.withTranslationUnit2
          index
          (Just $ SourcePath (Text.pack filename))
          clangArgs
          [unsavedFile]
          (bitfieldEnum [CXTranslationUnit_None])
          (const $ return Nothing)
          (fmap Just . process)
  where
    filename :: FilePath
    filename = "libclang-bindings-version.h"

    contents :: String
    contents = unlines [
        "const long long builtin_stdc = __STDC__;"
      , "const long long builtin_stdc_version = __STDC_VERSION__;"
      , "const long long builtin_linux = linux;"
      ]

    process :: CXTranslationUnit -> IO (Bool, Maybe Integer, Bool)
    process unit = do
      root <- clang_getTranslationUnitCursor unit
      kvs  <- HighLevel.clang_visitChildren root visit
      return
        ( lookupBool    "builtin_stdc"         kvs
        , lookupInteger "builtin_stdc_version" kvs
        , lookupBool    "linux"                kvs
        )

    visit :: Fold IO (Text, EvalResult)
    visit = simpleFold $ \curr -> do
      kind <- clang_getCursorKind curr
      case fromSimpleEnum kind of
        Right CXCursor_VarDecl ->
          clang_getCursorSpelling curr >>= \name ->
            HighLevel.clang_evaluate curr >>= \case
              Just er    -> foldContinueWith (name, er)
              _otherwise -> foldContinue
        _otherwise -> foldContinue

    lookupInteger :: Text -> [(Text, EvalResult)] -> Maybe Integer
    lookupInteger k kvs = case lookup k kvs of
      Just (EvalResultInteger n) -> Just n
      _otherwise                 -> Nothing

    lookupBool :: Text -> [(Text, EvalResult)] -> Bool
    lookupBool k = maybe False cToBool . lookupInteger k
