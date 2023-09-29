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
import Task qualified
import Text qualified

class MapError a b | a -> b where
  mapError :: a -> b

instance (ErrorMessage x, a ~ a') => MapError (Result x a) (Result Text a') where
  mapError = Result.mapError errorMessage

instance (ErrorMessage x, a ~ a') => MapError (Task x a) (Task Text a') where
  mapError = Task.mapError errorMessage

instance (a ~ a') => MapError (List a) (List a') where
  mapError = identity

instance (a ~ a') => MapError (Maybe a) (Maybe a') where
  mapError = identity

(>>) :: (MapError a a', Compose a' b c) => a -> b -> c
first >> second = mapError first OpenSolid.>> second

(>>=) :: (MapError a a', Bind a' b c) => a -> (b -> c) -> c
value >>= function = mapError value OpenSolid.>>= function

withContext :: (ErrorMessage x) => Text -> Result x a -> Result Text a
withContext context = Result.mapError (errorMessage OpenSolid.>> addContext context)

addContext :: Text -> Text -> Text
addContext context text = context ++ ":\n" ++ Text.indent "  " text
