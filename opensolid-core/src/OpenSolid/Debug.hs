module OpenSolid.Debug
  ( trace
  , show
  , log
  , intercept
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

show :: Show a => a -> Text
show = Text.show

log :: Show b => Text -> (a -> b) -> a -> a
log label function value = trace (label <> ": " <> show (function value)) value

intercept :: Show a => Text -> a -> a
intercept label value = log label id value

callStack :: HasCallStack => Text
callStack = do
  let callerCallStack = GHC.Stack.popCallStack GHC.Stack.callStack
  Text.pack (GHC.Stack.prettyCallStack callerCallStack)
