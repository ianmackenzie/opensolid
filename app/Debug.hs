module Debug (
    show,
    trace,
    log,
) where

import Debug.Trace qualified
import OpenSolid
import String qualified
import Prelude qualified

show :: Show a => a -> String
show value = String.fromList (Prelude.show value)

trace :: String -> a -> a
trace message value = Debug.Trace.trace (String.toList message) value

log :: Show a => String -> a -> a
log label value = trace (label ++ ": " ++ show value) value
