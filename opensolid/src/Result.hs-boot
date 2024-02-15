module Result (Result (..)) where

import {-# SOURCE #-} Error (Error)

data Result x a where
  Ok :: a -> Result x a
  Error :: Error x => x -> Result x a
