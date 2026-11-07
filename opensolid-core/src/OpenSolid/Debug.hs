module OpenSolid.Debug
  ( (!)
  , trace
  , show
  , Log
  , log
  , callStack
  )
where

import Debug.Trace qualified
import GHC.Stack (HasCallStack)
import GHC.Stack qualified
import GHC.TypeLits (KnownSymbol)
import OpenSolid.Prelude hiding (show)
import OpenSolid.Text qualified as Text

trace :: Text -> Log
trace message = Log [message]

show :: Show a => a -> Text
show = Text.show

data Log = Log (List Text)

(!) :: a -> Log -> a
value ! Log messages = Debug.Trace.trace (string (Text.multiline messages)) value

infixl 0 !

log :: Log
log = Log []

instance
  (KnownSymbol name, Show value) =>
  SetField name value Log Log
  where
  setField value (Log lines) = do
    Log (lines <> [Text.symbol name <> ": " <> show value])

callStack :: HasCallStack => Text
callStack = do
  let callerCallStack = GHC.Stack.popCallStack GHC.Stack.callStack
  fromString (GHC.Stack.prettyCallStack callerCallStack)
