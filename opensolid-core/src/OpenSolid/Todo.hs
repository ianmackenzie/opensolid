module OpenSolid.Todo (pattern TODO) where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Prelude (error)

pattern TODO :: HasCallStack => a
pattern TODO <- (withFrozenCallStack todo -> ())
  where
    TODO = withFrozenCallStack todo

todo :: a
todo = error "Not implemented"

{-# COMPLETE TODO #-}
