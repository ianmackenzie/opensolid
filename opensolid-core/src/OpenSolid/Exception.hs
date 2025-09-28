module OpenSolid.Exception (higherOrderZero) where

import OpenSolid.Bootstrap

higherOrderZero :: HasCallStack => a
higherOrderZero = exception "Higher-order zero detected"
