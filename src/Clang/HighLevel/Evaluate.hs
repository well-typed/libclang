module Clang.HighLevel.Evaluate (
    -- * Types
    EvalResult(..)
    -- * API
  , clang_evaluate
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign.C qualified as C

import Clang.Enum.Simple
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Evaluation result
data EvalResult =
    -- | Result as an 'Integer'
    EvalResultInteger Integer
  | -- | Result as a 'C.CDouble'
    EvalResultCDouble C.CDouble
  | -- | Result as a 'String'
    EvalResultString  String
  deriving stock (Show)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Evaluate a statement, variable (initializer), or expression
--
-- LLVM/Clang documentation is sparse, and enumeration 'CXEvalResultKind'
-- specifies kinds that do not exactly match the API for getting evaluation
-- results.  Any use of this function should be tested thoroughly.
clang_evaluate :: (MonadIO m, MonadMask m) => CXCursor -> m (Maybe EvalResult)
clang_evaluate cursor =
    bracket (clang_Cursor_Evaluate cursor) clang_EvalResult_dispose $ \er ->
      (fromSimpleEnum <$> clang_EvalResult_getKind er) >>= \case
        Left{} -> return Nothing
        Right CXEval_UnExposed -> return Nothing
        Right CXEval_Int -> fmap (Just . EvalResultInteger) $ do
          isUnsigned <- clang_EvalResult_isUnsignedInt er
          if isUnsigned /= 0
            then toInteger <$> clang_EvalResult_getAsUnsigned er
            else toInteger <$> clang_EvalResult_getAsLongLong er
        Right CXEval_Float ->
          Just . EvalResultCDouble <$> clang_EvalResult_getAsDouble er
        Right{} ->
          Just . EvalResultString <$> clang_EvalResult_getAsStr er
