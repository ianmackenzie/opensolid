module Debug (
    show,
    trace,
    log,
) where

import Debug.Trace qualified
import OpenSolid
import Text qualified
import Prelude qualified

show :: Show a => a -> Text
show value = Text.fromChars (Prelude.show value)

trace :: Text -> a -> a
trace message = Debug.Trace.trace (Text.toChars message)

log :: Show a => Text -> a -> a
log label value = trace (label ++ ": " ++ show value) value
