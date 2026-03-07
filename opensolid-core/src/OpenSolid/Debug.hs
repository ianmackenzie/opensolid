module OpenSolid.Debug
  ( trace
  , log
  , callStack
  )
where

import Debug.Trace qualified
import GHC.Stack (HasCallStack)
import GHC.Stack qualified
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

trace :: Text -> a -> a
trace message value = Debug.Trace.trace (Text.unpack message) value

log :: Show a => Text -> a -> a
log label value = trace (label <> ": " <> Text.show value) value

callStack :: HasCallStack => Text
callStack = do
  let callerCallStack = GHC.Stack.popCallStack GHC.Stack.callStack
  Text.pack (GHC.Stack.prettyCallStack callerCallStack)
