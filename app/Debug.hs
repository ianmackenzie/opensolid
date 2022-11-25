module Debug (
    toString,
    trace,
    log,
) where

import qualified Debug.Trace
import OpenSolid
import qualified String

toString :: Show a => a -> String
toString value =
    String.fromList (show value)

trace :: String -> a -> a
trace message value =
    Debug.Trace.trace (String.toList message) value

log :: Show a => String -> a -> a
log label value =
    trace (label ++ ": " ++ toString value) value
