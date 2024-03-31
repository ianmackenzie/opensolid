module Result (Result (..)) where

import Basics
import Coalesce
import {-# SOURCE #-} Error (Error)

data Result x a where
  Ok :: a -> Result x a
  Error :: Error x => x -> Result x a

instance a ~ a' => Coalesce (Result x a) (Maybe a') (Maybe a)

instance a ~ a' => Coalesce (Maybe a) (Result x a') (Result x a)
