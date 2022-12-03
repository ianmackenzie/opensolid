module Debug (
    show,
    trace,
    log,
) where

import qualified Debug.Trace
import OpenSolid
import qualified String
import qualified Prelude

show :: Show a => a -> String
show value = String.fromList (Prelude.show value)

trace :: String -> a -> a
trace message value = Debug.Trace.trace (String.toList message) value

log :: Show a => String -> a -> a
log label value = trace (label ++ ": " ++ show value) value
