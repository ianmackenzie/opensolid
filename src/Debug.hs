module Debug
  ( show
  , trace
  , log
  , intercept
  )
where

import Basics
import Concatenate
import Data.Text qualified
import Debug.Trace qualified
import Prelude qualified

show :: Show a => a -> Text
show value = Data.Text.pack (Prelude.show value)

trace :: Text -> a -> a
trace message = Debug.Trace.trace (Data.Text.unpack message)

log :: Show a => Text -> a -> b -> b
log label value = trace (label ++ ": " ++ show value)

intercept :: Show a => Text -> a -> a
intercept label value = log label value value
