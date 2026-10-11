module OpenSolid.Try (Bind ((>>=)), (>>)) where

import OpenSolid.Err qualified as Err
import OpenSolid.Prelude hiding ((>>), (>>=))
import Prelude qualified

class Bind m1 m2 where
  (>>=) :: m1 a -> (a -> m2 b) -> m2 b

instance MonadFail m => Bind (Result x) m where
  Ok value >>= function = function value
  Err err >>= _ = Prelude.fail (string (Err.message err))

instance Bind IO IO where
  (>>=) = (Prelude.>>=)

(>>) :: Bind m1 m2 => m1 () -> m2 a -> m2 a
first >> second = first >>= \() -> second
