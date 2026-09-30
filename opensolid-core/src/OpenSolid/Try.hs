module OpenSolid.Try (Bind ((>>=)), (>>)) where

import OpenSolid.Err qualified as Err
import OpenSolid.Monad qualified as Monad
import OpenSolid.Prelude hiding ((>>), (>>=))
import Prelude qualified

class Bind m1 m2 where
  (>>=) :: m1 a -> (a -> m2 b) -> m2 b

instance x2 ~ Text => Bind (Result x1) (Result x2) where
  Ok value >>= function = function value
  Err err >>= _ = Err (Err.message err)

instance Bind (Result x) IO where
  Ok value >>= function = function value
  Err err >>= _ = Monad.fail (Err.message err)

instance Bind IO IO where
  (>>=) = (Prelude.>>=)

(>>) :: Bind m1 m2 => m1 () -> m2 a -> m2 a
first >> second = first >>= \() -> second
