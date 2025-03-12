module OpenSolid.Debug
  ( Debug
  , print
  , log
  , assert
  , trace
  , intercept
  , callStack
  )
where

import Control.Exception qualified
import Data.Text qualified
import Debug.Trace qualified
import GHC.Stack qualified
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Text qualified as Text
import Prelude qualified

newtype Debug = Debug (() -> ())

instance Composition Debug a a where
  Debug action >> value = Prelude.seq (action ()) value

labelled :: Show a => Text -> a -> Text
labelled label value = label <> ": " <> Text.show value

print :: Text -> Debug
print message = Debug (trace message)

log :: Show a => Text -> a -> Debug
log label value = print (labelled label value)

assert :: HasCallStack => Bool -> Debug
assert condition = Debug (Control.Exception.assert condition)

trace :: Text -> a -> a
trace = Data.Text.unpack >> Debug.Trace.trace

intercept :: Show a => Text -> a -> a
intercept label value = trace (labelled label value) value

callStack :: HasCallStack => Text
callStack = do
  let callerCallStack = GHC.Stack.popCallStack GHC.Stack.callStack
  Text.pack (GHC.Stack.prettyCallStack callerCallStack)
