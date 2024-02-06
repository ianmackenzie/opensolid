module Composition (Composition ((>>)), (<<)) where

import Data.Type.Equality (type (~))
import Prelude (IO)
import Prelude qualified

class Composition a b c | a b -> c, a c -> b where
  (>>) :: a -> b -> c

(<<) :: Composition a b c => b -> a -> c
b << a = a >> b

infixl 9 >>

infixr 9 <<

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  f >> g = g Prelude.. f

instance m ~ IO => Composition (IO a) (m b) (m b) where
  (>>) = (Prelude.>>)
