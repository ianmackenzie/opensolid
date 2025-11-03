module OpenSolid.Debug
  ( Debug
  , (>>)
  , print
  , log
  , trace
  , intercept
  , callStack
  )
where

import Data.Text qualified
import Debug.Trace qualified
import GHC.Stack qualified
import OpenSolid.Bootstrap hiding (print, (>>))
import OpenSolid.Text qualified as Text

newtype Debug = Debug (() -> ())

(>>) :: Debug -> a -> a
Debug action >> value = seq (action ()) value

labelled :: Show a => Text -> a -> Text
labelled label value = label <> ": " <> Text.show value

print :: Text -> Debug
print message = Debug (trace message)

log :: Show a => Text -> a -> Debug
log label value = print (labelled label value)

trace :: Text -> a -> a
trace text = Debug.Trace.trace (Data.Text.unpack text)

intercept :: Show a => Text -> a -> a
intercept label value = trace (labelled label value) value

callStack :: HasCallStack => Text
callStack = do
  let callerCallStack = GHC.Stack.popCallStack GHC.Stack.callStack
  Text.pack (GHC.Stack.prettyCallStack callerCallStack)
