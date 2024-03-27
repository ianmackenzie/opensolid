module Composition (Composition ((>>))) where

import Prelude (IO, type (~))
import Prelude qualified

class Composition a b c | a b -> c where
  (>>) :: a -> b -> c

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  f >> g = g Prelude.. f

instance Composition (IO ()) (IO a) (IO a) where
  (>>) = (Prelude.>>)
