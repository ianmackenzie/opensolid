module Debug
  ( trace
  , log
  , intercept
  , assert
  )
where

import Basics
import Concatenation
import Control.Exception (assert)
import Debug.Trace qualified

trace :: String -> a -> a
trace = Debug.Trace.trace

log :: (Show a) => String -> a -> b -> b
log label value = trace (label ++ ": " ++ show value)

intercept :: (Show a) => String -> a -> a
intercept label value = log label value value
