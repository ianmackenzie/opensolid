module OpenSolid.Composition (Composition ((.), compose)) where

import Prelude (type (~))
import Prelude qualified

class Composition a b c | a b -> c where
  (.) :: b -> a -> c
  compose :: b -> a -> c

  (.) = compose
  compose = (.)

  {-# MINIMAL (.) | compose #-}

infixr 9 .

infixr 9 `compose`

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  (.) = (Prelude..)
