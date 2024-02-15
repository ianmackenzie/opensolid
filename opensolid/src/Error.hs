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

class (Error x, Error y) => Map x y a b | a -> x, b -> y where
  map :: (x -> y) -> a -> b

toString :: Map x String a b => a -> b
toString = map message

context :: Map x String a b => String -> a -> b
context string = map (message >> addContext string)

addContext :: String -> String -> String
addContext string text = string ++ ":\n" ++ String.indent "  " text
