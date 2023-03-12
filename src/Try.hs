module Try
  ( (>>=)
  , fail
  , withContext
  )
where

import OpenSolid hiding ((>>=))
import Result qualified
import Text qualified

(>>=) :: IsError x => Result x a -> (a -> Result Text b) -> Result Text b
Ok value >>= function = function value
Error error >>= _ = Error (errorMessage error)

withContext :: IsError x => Text -> Result x a -> Result Text a
withContext context result = result |> Result.mapError (errorMessage >> addContext context)

addContext :: Text -> Text -> Text
addContext context text = context ++ "\n  " ++ Text.replace "\n" "\n  " text
