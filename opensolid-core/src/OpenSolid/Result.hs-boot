module OpenSolid.Result (Result (Success, Failure)) where

import Prelude (Show)

data Result x a where
  Success :: a -> Result x a
  Failure :: Show x => x -> Result x a
