module Clang.HighLevel.Declaration (
    -- * Declaration
    DeclarationClassification(..)
  , classifyDeclaration
    -- * Other
  , classifyTentativeDefinition
    -- * Availability
  , Availability(..)
  , clang_getCursorAvailability
  ) where

import Control.Monad.IO.Class
import GHC.Generics (Generic)

import Clang.Enum.Simple
import Clang.LowLevel.Core (CXCursor, CX_StorageClass(..), CXAvailabilityKind(..))
import Clang.LowLevel.Core qualified as LowLevel

{-------------------------------------------------------------------------------
  Declaration
-------------------------------------------------------------------------------}

-- | Declaration classification
--
-- This classification function is suitable for declarations of functions,
-- variables, enums, structs, and unions.
--
-- Forward declarations and redeclarations can be classified as either
-- 'DefinitionElsewhere' or 'DefinitionUnavailable'.
--
-- <https://en.cppreference.com/w/c/language/struct.html#Forward_declaration>
--
-- <https://en.cppreference.com/w/c/language/declarations.html#Redeclaration>
--
-- Despite the name, a tentative definition is /not/ classified as a
-- 'Definition'. Use 'classifyTentativeDefinition' to detect whether a
-- declaration is a tentative definition.
--
-- <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
data DeclarationClassification =
    -- | A declaration together with a definition.
    --
    -- > int foo (void) { return 1; }; // cursor positioned here
    --
    -- <https://en.cppreference.com/w/c/language/declarations.html#Definitions>
    Definition

    -- | A declaration without definition, but the definition is available
    -- elsewhere in the translation unit.
    --
    -- > struct X; // cursor positioned here
    -- > struct X { int n; };
  | DefinitionElsewhere CXCursor

    -- | A declaration without a definition, and there is no definition
    -- available elsewhere in the translation unit.
    --
    -- > extern int x; // cursor positioned here
  | DefinitionUnavailable
  deriving stock (Show, Eq)

-- | Classify a declaration
classifyDeclaration ::
     MonadIO m
  => CXCursor  -- ^ Declaration
  -> m DeclarationClassification
classifyDeclaration cursor = do
    defnCursor <- LowLevel.clang_getCursorDefinition cursor
    isDefnNull <- LowLevel.clang_equalCursors defnCursor LowLevel.nullCursor
    if isDefnNull
      then return DefinitionUnavailable
      else do
        isCursorDefn <- LowLevel.clang_equalCursors cursor defnCursor
        return $
          if isCursorDefn
            then Definition
            else DefinitionElsewhere defnCursor

{-------------------------------------------------------------------------------
  Other
-------------------------------------------------------------------------------}

-- | Classify whether a declaration of a global variable is a tentative
-- definition.
--
-- NOTE: this function assumes that the cursor points to a global variable
-- declaration.
--
-- A tentative definition is an external declaration without an initializer,
-- and either without a storage-class specifier or with the specifier static.
--
-- A tentative definition is a declaration that may or may not act as a
-- definition. If an actual external definition is found earlier or later in the
-- same translation unit, then the tentative definition just acts as a
-- declaration.
--
-- <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
classifyTentativeDefinition ::
     MonadIO m
  => CXCursor
  -> m Bool
classifyTentativeDefinition cursor = do
    initrCursor <- LowLevel.clang_Cursor_getVarDeclInitializer cursor
    isInitrNull <- LowLevel.clang_equalCursors initrCursor LowLevel.nullCursor
    if isInitrNull
      then do
        storage <- LowLevel.clang_Cursor_getStorageClass cursor
        case fromSimpleEnum storage of
          Right CX_SC_Static -> pure True
          Right CX_SC_None -> pure True
          _ -> pure False
      else pure False

{-------------------------------------------------------------------------------
  Availability
-------------------------------------------------------------------------------}

-- | Describes the availability of a particular entity, which indicates whether
-- the use of this entity will result in a warning or error due to it being
-- deprecated or unavailable.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX.html#gada331ea0195e952c8f181ecf15e83d71>
data Availability =
     -- | The entity is available.
    Available
     -- | The entity is available, but has been deprecated (and its use is not
     -- recommended).
  | Deprecated
     -- | The entity is not available; any use of it will be an error.
  | NotAvailable
     -- | The entity is available, but not accessible; any use of it will be an
     -- error.
  | NotAccessible
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Determine the availability of the entity that this cursor refers to, taking
-- the current target platform into account.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gab44e2a565fa40a0e0fc0f130f618a9b5>
clang_getCursorAvailability :: (MonadIO m) => CXCursor -> m Availability
clang_getCursorAvailability cursor = do
    kind <- LowLevel.clang_getCursorAvailability cursor
    case fromSimpleEnum kind of
      Right k -> pure $ toAvailability k
      Left  _ -> error $ "unexpected cursor kind: " ++ show kind
  where
    toAvailability = \case
      CXAvailability_Available     -> Available
      CXAvailability_Deprecated    -> Deprecated
      CXAvailability_NotAvailable  -> NotAvailable
      CXAvailability_NotAccessible -> NotAccessible
