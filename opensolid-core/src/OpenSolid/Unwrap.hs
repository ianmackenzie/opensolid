module OpenSolid.Unwrap (Unwrap (unwrap)) where

import Prelude (Maybe, id)

class Unwrap m where
  unwrap :: m a -> Maybe a

instance Unwrap Maybe where
  {-# INLINE unwrap #-}
  unwrap = id
