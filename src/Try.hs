module Try
  ( (>>=)
  , (>>)
  , fail
  , withContext
  )
where

import List qualified
import OpenSolid hiding (Bind ((>>=)), Compose ((>>)))
import OpenSolid qualified
import Result qualified
import Script (Script)
import Script qualified
import Text qualified

class Compose a b c | a b -> c where
  (>>) :: a -> b -> c

instance IsError x => Compose (Script x ()) (Script Text a) (Script Text a) where
  script1 >> script2 = Script.mapError errorMessage script1 OpenSolid.>> script2

class Bind a b c | a b -> c where
  (>>=) :: a -> b -> c

instance
  (IsError x, a ~ a')
  => Bind
      (Result x a)
      (a' -> List b)
      (Result Text (List b))
  where
  Ok value >>= function = Ok (function value)
  Error error >>= _ = Error (errorMessage error)

instance
  (IsError x, a ~ a')
  => Bind
      (Result x (List a))
      (a' -> List b)
      (Result Text (List b))
  where
  Error error >>= _ = Error (errorMessage error)
  Ok items >>= function = Ok (List.combine function items)

instance
  (IsError x, a ~ a')
  => Bind
      (List a)
      (a' -> Result x (List b))
      (Result Text (List b))
  where
  list >>= function = Result.map List.concat (List.collate (List.map (function OpenSolid.>> Result.mapError errorMessage) list))

instance
  (IsError x, a ~ a', text ~ Text)
  => Bind
      (Result x a)
      (a' -> Result text b)
      (Result Text b)
  where
  Ok value >>= function = function value
  Error error >>= _ = Error (errorMessage error)

instance
  (IsError x, a ~ a', text ~ Text)
  => Bind
      (Result x a)
      (a' -> Script text b)
      (Script Text b)
  where
  Ok value >>= function = function value
  Error error >>= _ = Script.fail (errorMessage error)

instance
  (IsError x, a ~ a')
  => Bind
      (Script x a)
      (a' -> Script Text b)
      (Script Text b)
  where
  script >>= function = Script.mapError errorMessage script OpenSolid.>>= function

withContext :: IsError x => Text -> Result x a -> Result Text a
withContext context result = result |> Result.mapError (errorMessage OpenSolid.>> addContext context)

addContext :: Text -> Text -> Text
addContext context text = context ++ "\n  " ++ Text.replace "\n" "\n  " text
