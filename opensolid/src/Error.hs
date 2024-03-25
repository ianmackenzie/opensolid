module Error
  ( Error (message)
  , Map (map)
  , toString
  , context
  , debug
  , trace
  , log
  )
where

import Basics
import Concatenation
import Debug qualified
import {-# SOURCE #-} IO qualified
import String qualified
import Prelude qualified

class (Eq error, Show error) => Error error where
  message :: error -> String
  message = show

instance Error String where
  message = identity

class (Error x, Error y) => Map x y m n | m -> x, n -> y, m y -> n where
  map :: (x -> y) -> m a -> n a

instance Map String String IO IO where
  map function = IO.onError (\error -> Prelude.fail (function error))

toString :: Map x String m n => m a -> n a
toString = map message

context :: Map x String m n => String -> m a -> n a
context string = map (message >> addContext string)

addContext :: String -> String -> String
addContext string text = string ++ ":\n" ++ String.indent "  " text

debug :: Map x x m m => (x -> IO ()) -> m a -> m a
debug callback = map (\error -> Debug.io (callback error) error)

trace :: Map x x m m => String -> m a -> m a
trace output = map (Debug.trace output)

log :: (Map x x m m, Show b) => String -> b -> m a -> m a
log label value = map (Debug.log label value)
