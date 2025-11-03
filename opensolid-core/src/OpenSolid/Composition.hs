module OpenSolid.Composition (Composition ((.))) where

import Prelude (type (~))
import Prelude qualified

class Composition a b c | a b -> c where
  (.) :: b -> a -> c

infixr 9 .

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  (.) = (Prelude..)
