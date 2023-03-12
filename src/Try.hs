module Try
  ( (>>=)
  , (>>)
  , fail
  , withContext
  )
where

import OpenSolid hiding (Bind ((>>=)), Compose ((>>)))
import OpenSolid qualified
import Result qualified
import Script (Script)
import Script qualified
import Text qualified

class MapError p q | p -> q where
  mapError :: p a -> q a

instance IsError x => MapError (Result x) (Result Text) where
  mapError = Result.mapError errorMessage

instance IsError x => MapError (Script x) (Script Text) where
  mapError = Script.mapError errorMessage

instance MapError [] [] where
  mapError = identity

instance MapError Maybe Maybe where
  mapError = identity

(>>) :: (MapError p q, OpenSolid.Compose (q a) b c) => p a -> b -> c
first >> second = mapError first OpenSolid.>> second

(>>=) :: (MapError p q, OpenSolid.Bind q b) => p a -> (a -> b) -> b
value >>= function = mapError value OpenSolid.>>= function

withContext :: IsError x => Text -> Result x a -> Result Text a
withContext context result = result |> Result.mapError (errorMessage OpenSolid.>> addContext context)

addContext :: Text -> Text -> Text
addContext context text = context ++ "\n  " ++ Text.replace "\n" "\n  " text
