module Try
  ( (>>=)
  , (>>)
  , fail
  , withContext
  )
where

import OpenSolid hiding ((>>), (>>=))
import OpenSolid qualified
import Result qualified
import String qualified
import Task qualified

class (Monad m1, Monad m2) => MapError m1 m2 | m1 -> m2 where
  mapError :: m1 a -> m2 a

instance (ErrorMessage x) => MapError (Result x) (Result String) where
  mapError = Result.mapError errorMessage

instance (ErrorMessage x, a ~ a') => MapError (Task x) (Task String) where
  mapError = Task.mapError errorMessage

instance (a ~ a') => MapError [] [] where
  mapError = identity

instance (a ~ a') => MapError Maybe Maybe where
  mapError = identity

(>>) :: (MapError m1 m2) => m1 a -> m2 b -> m2 b
first >> second = mapError first OpenSolid.>> second

(>>=) :: (MapError m1 m2) => m1 a -> (a -> m2 b) -> m2 b
value >>= function = mapError value OpenSolid.>>= function

withContext :: (ErrorMessage x) => String -> Result x a -> Result String a
withContext context = Result.mapError (addContext context . errorMessage)

addContext :: String -> String -> String
addContext context text = context ++ ":\n" ++ String.indent "  " text
