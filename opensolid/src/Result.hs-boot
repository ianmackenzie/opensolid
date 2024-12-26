module Result (Result (Success, Failure)) where

import {-# SOURCE #-} Error qualified

data Result x a where
  Success :: a -> Result x a
  Failure :: Error.Message x => x -> Result x a
