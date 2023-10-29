module Debug
  ( show
  , trace
  , log
  , intercept
  )
where

import Basics
import Concatenate
import Debug.Trace qualified
import Prelude qualified

show :: (Show a) => a -> String
show = Prelude.show

trace :: String -> a -> a
trace = Debug.Trace.trace

log :: (Show a) => String -> a -> b -> b
log label value = trace (label ++ ": " ++ show value)

intercept :: (Show a) => String -> a -> a
intercept label value = log label value value
