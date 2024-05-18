module Debug
  ( Debug
  , print
  , log
  , assert
  , io
  , trace
  , intercept
  )
where

import Arithmetic
import Basics
import Composition
import Control.Exception qualified
import Debug.Trace qualified
import GHC.Stack (HasCallStack)
import System.IO.Unsafe qualified
import Prelude qualified

newtype Debug = Debug (() -> ())

instance Composition Debug a a where
  Debug action >> value = Prelude.seq (action ()) value

labelled :: Show a => String -> a -> String
labelled label value = label + ": " + show value

print :: String -> Debug
print message = Debug (trace message)

log :: Show a => String -> a -> Debug
log label value = print (labelled label value)

assert :: HasCallStack => Bool -> Debug
assert condition = Debug (Control.Exception.assert condition)

io :: IO () -> Debug
io debugIO = Debug (Prelude.seq (System.IO.Unsafe.unsafePerformIO debugIO))

trace :: String -> a -> a
trace = Debug.Trace.trace

intercept :: Show a => String -> a -> a
intercept label value = trace (labelled label value) value
