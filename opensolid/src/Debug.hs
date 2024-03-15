module Debug
  ( trace
  , log
  , intercept
  , assert
  , task
  , sequence
  )
where

import Basics
import Concatenation
import Control.Exception (assert)
import Debug.Trace qualified
import System.IO.Unsafe qualified
import {-# SOURCE #-} Task (Task)
import {-# SOURCE #-} Task qualified
import Prelude qualified

trace :: String -> a -> a
trace = Debug.Trace.trace

log :: Show a => String -> a -> b -> b
log label value = trace (label ++ ": " ++ show value)

intercept :: Show a => String -> a -> a
intercept label value = log label value value

task :: Task () -> a -> a
task debugTask value =
  System.IO.Unsafe.unsafePerformIO Prelude.do
    Task.toIO debugTask
    Prelude.return value

sequence :: List (a -> a) -> a -> a
sequence [] = identity
sequence (first : rest) = first << sequence rest
