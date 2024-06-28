module Composition (Composition ((>>)), (.)) where

import Prelude (IO, type (~))
import Prelude qualified

class Composition a b c | a b -> c where
  (>>) :: a -> b -> c
  (.) :: b -> a -> c
  a >> b = b . a
  a . b = b >> a
  {-# MINIMAL (>>) | (.) #-}

infixl 9 >>

infixr 9 .

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  (.) = (Prelude..)

instance Composition (IO ()) (IO a) (IO a) where
  (>>) = (Prelude.>>)
