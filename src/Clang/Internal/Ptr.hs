-- | Pointer utilities
module Clang.Internal.Ptr (
    safeCastPtr
  ) where

import Data.Coerce (Coercible, coerce)
import Foreign.Ptr (Ptr, castPtr)

-- | Safely casts a 'Ptr' to a 'Ptr' of a different type.
safeCastPtr ::
  forall a b. Coercible a b => Ptr a -> Ptr b
safeCastPtr = castPtr
  where
    _unused :: a -> b
    _unused = coerce
