module Debug
  ( trace
  , log
  , intercept
  , assert
  , io
  , sequence
  )
where

import Basics
import Concatenation
import Control.Exception (assert)
import Debug.Trace qualified
import System.IO.Unsafe qualified
import Prelude qualified

trace :: String -> a -> a
trace = Debug.Trace.trace

log :: Show a => String -> a -> b -> b
log label value = trace (label ++ ": " ++ show value)

intercept :: Show a => String -> a -> a
intercept label value = log label value value

io :: IO () -> a -> a
io debugIO value =
  System.IO.Unsafe.unsafePerformIO Prelude.do
    debugIO
    Prelude.return value

sequence :: List (a -> a) -> a -> a
sequence [] = identity
sequence (first : rest) = first << sequence rest
