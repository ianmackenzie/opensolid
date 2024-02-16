module Error
  ( Error (message)
  , Map (map)
  , toString
  , context
  )
where

import Basics
import Concatenation
import String qualified

class (Eq error, Show error) => Error error where
  message :: error -> String
  message = show

instance Error String where
  message = identity

class (Error x, Error y) => Map x y m n | m -> x, n -> y, m y -> n where
  map :: (x -> y) -> m a -> n a

toString :: Map x String m n => m a -> n a
toString = map message

context :: Map x String m n => String -> m a -> n a
context string = map (message >> addContext string)

addContext :: String -> String -> String
addContext string text = string ++ ":\n" ++ String.indent "  " text
