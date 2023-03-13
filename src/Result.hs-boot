module Result
  ( IsError (errorMessage)
  , Result (Ok, Error)
  , map
  )
where

import Basics

class IsError error where
  errorMessage :: error -> Text

instance IsError Text

data Result x a where
  Ok :: a -> Result x a
  Error :: IsError x => x -> Result x a

map :: (a -> b) -> Result x a -> Result x b
