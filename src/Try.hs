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

class MapError p q | p -> q where
  mapError :: p a -> q a

instance IsError x => MapError (Result x) (Result Text) where
  mapError = Result.mapError errorMessage

instance IsError x => MapError (Task x) (Task Text) where
  mapError = Task.mapError errorMessage

instance MapError [] [] where
  mapError = identity

instance MapError Maybe Maybe where
  mapError = identity

(>>) :: (MapError p q, Compose (q a) b c) => p a -> b -> c
first >> second = mapError first OpenSolid.>> second

(>>=) :: (MapError p q, Bind q b) => p a -> (a -> b) -> b
value >>= function = mapError value OpenSolid.>>= function

withContext :: IsError x => Text -> Result x a -> Result Text a
withContext context = Result.mapError (errorMessage OpenSolid.>> addContext context)

addContext :: Text -> Text -> Text
addContext context text = context ++ "\n  " ++ Text.replace "\n" "\n  " text
