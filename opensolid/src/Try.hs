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

class Monad (m String) => MapError m where
  mapError :: ErrorMessage x => m x a -> m String a

instance MapError Result where
  mapError = Result.mapError errorMessage

instance MapError Task where
  mapError = Task.mapError errorMessage

(>>) :: (MapError m, ErrorMessage x) => m x a -> m String b -> m String b
first >> second = mapError first OpenSolid.>> second

(>>=) :: (MapError m, ErrorMessage x) => m x a -> (a -> m String b) -> m String b
value >>= function = mapError value OpenSolid.>>= function

withContext :: ErrorMessage x => String -> Result x a -> Result String a
withContext context = Result.mapError (addContext context . errorMessage)

addContext :: String -> String -> String
addContext context text = context ++ ":\n" ++ String.indent "  " text
