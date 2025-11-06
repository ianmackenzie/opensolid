module OpenSolid.Exception (higherOrderZero) where

import GHC.Stack (HasCallStack)
import Prelude (error)

higherOrderZero :: HasCallStack => a
higherOrderZero = error "Higher-order zero detected"
