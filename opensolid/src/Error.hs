module Error
  ( Error (message)
  , Map (map)
  , toString
  , context
  , print
  , log
  , debug
  )
where

import Basics
import Composition
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

print :: Map x x m m => String -> m a -> m a
print output = map (\error -> do Debug.print output; error)

log :: (Map x x m m, Show b) => String -> b -> m a -> m a
log label value = map (\error -> do Debug.log label value; error)

debug :: Map x x m m => (x -> IO ()) -> m a -> m a
debug callback = map (\error -> do Debug.io (callback error); error)
