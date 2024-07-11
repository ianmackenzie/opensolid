module Result (Result (Success, Failure)) where

import Basics
import Coalesce
import {-# SOURCE #-} Error qualified

data Result x a where
  Success :: a -> Result x a
  Failure :: Error.Message x => x -> Result x a

instance a1 ~ a2 => Coalesce (Maybe a1) (Result error a2) (Result error a1)
