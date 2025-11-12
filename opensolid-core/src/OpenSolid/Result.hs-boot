module OpenSolid.Result (Result (Ok, Error)) where

import Prelude (Show)

data Result x a where
  Ok :: a -> Result x a
  Error :: Show x => x -> Result x a
