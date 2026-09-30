module OpenSolid.Monad (return, fail) where

import OpenSolid.Prelude
import Prelude (return)
import Prelude qualified

{-# INLINE fail #-}
fail :: MonadFail m => Text -> m a
fail = Prelude.fail . string
