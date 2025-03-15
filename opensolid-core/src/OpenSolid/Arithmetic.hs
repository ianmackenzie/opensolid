module OpenSolid.Arithmetic
  ( Negation (negate)
  , Addition ((+))
  , Subtraction ((-))
  , subtract
  , Multiplication' ((.*.))
  , Multiplication ((*))
  , Division' ((./.))
  , Division ((/))
  , DivMod ((//), (%))
  , DotMultiplication' (dot')
  , DotMultiplication (dot)
  , CrossMultiplication' (cross')
  , CrossMultiplication (cross)
  , Exponentiation ((**))
  , (*!)
  , (!*)
  , (^*.)
  , (.*^)
  , (!/)
  , (/%)
  , (.!/!)
  , (!./!)
  , (!/!.)
  , (!/.!)
  , (.!/.!)
  , (./^)
  , (!?/.!?)
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Float (Float)
import {-# SOURCE #-} OpenSolid.Qty (Qty (Qty))
import {-# SOURCE #-} OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.SymbolicOperators
import OpenSolid.Units (HasUnits, Unitless)
import OpenSolid.Units qualified as Units
import Prelude qualified

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negate :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

infixl 6 +

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

infixl 6 -

subtract :: Subtraction a b c => b -> a -> c
subtract b a = a - b

class Multiplication' a b c | a b -> c where
  (.*.) :: a -> b -> c

infixl 7 .*.

class Multiplication b a c => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

infixl 7 *

class Division' a b c | a b -> c where
  (./.) :: a -> b -> c

infixl 7 ./.

class Division a b c | a b -> c where
  (/) :: a -> b -> c

infixl 7 /

class DotMultiplication' a b c | a b -> c where
  dot' :: a -> b -> c

infixl 7 `dot'`

class DotMultiplication b a c => DotMultiplication a b c | a b -> c where
  dot :: DotMultiplication a b c => a -> b -> c

infixl 7 `dot`

class CrossMultiplication' a b c | a b -> c where
  cross' :: a -> b -> c

infixl 7 `cross'`

class CrossMultiplication b a c => CrossMultiplication a b c | a b -> c where
  cross :: a -> b -> c

infixl 7 `cross`

class DivMod a where
  (//) :: a -> a -> Int
  (%) :: a -> a -> a

infixl 7 //

infixl 7 %

instance DivMod Int where
  (//) = Prelude.div
  (%) = Prelude.mod

instance Negation Int where
  negate = Prelude.negate

instance Multiplication' Sign Int Int where
  {-# INLINEABLE (.*.) #-}
  Positive .*. n = n
  Negative .*. n = -n

instance Multiplication Sign Int Int where
  {-# INLINEABLE (*) #-}
  Positive * n = n
  Negative * n = -n

instance Multiplication' Int Sign Int where
  {-# INLINEABLE (.*.) #-}
  n .*. Positive = n
  n .*. Negative = -n

instance Multiplication Int Sign Int where
  {-# INLINEABLE (*) #-}
  n * Positive = n
  n * Negative = -n

instance Addition Int Int Int where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)

instance Subtraction Int Int Int where
  (-) = (Prelude.-)

instance Multiplication' Int Int Int where
  (.*.) = (Prelude.*)

instance Multiplication Int Int Int where
  (*) = (Prelude.*)

instance Division' Int Int Float where
  n ./. m = Qty (fromIntegral n Prelude./ fromIntegral m)

instance Division Int Int Float where
  n / m = Qty (fromIntegral n Prelude./ fromIntegral m)

class Exponentiation a b where
  (**) :: a -> b -> a

infixr 8 **

instance Exponentiation Int Int where
  (**) = (Prelude.^)

instance units ~ Unitless => Exponentiation (Qty units) Int where
  x ** n = x ** (fromIntegral n :: Qty Unitless)

instance
  (baseUnits ~ Unitless, exponentUnits ~ Unitless) =>
  Exponentiation (Qty baseUnits) (Qty exponentUnits)
  where
  (**) = (Prelude.**)

-- GENERIC UNITS MANIPULATION

(*!) ::
  ( Multiplication' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1 :/: units2
  , unitsB ~ units2
  , unitsC ~ units1
  ) =>
  a ->
  b ->
  c
a *! b = Units.coerce (a .*. b)

infixl 7 *!

(!*) ::
  ( Multiplication' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units2
  , unitsB ~ units1 :/: units2
  , unitsC ~ units1
  ) =>
  a ->
  b ->
  c
a !* b = Units.coerce (a .*. b)

infixl 7 !*

(^*.) ::
  ( Multiplication' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1
  , unitsB ~ Unitless :/: units2
  , unitsC ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a ^*. b = Units.coerce (a .*. b)

infixl 7 ^*.

(.*^) ::
  ( Multiplication' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ Unitless :/: units2
  , unitsB ~ units1
  , unitsC ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .*^ b = Units.coerce (a .*. b)

infixl 7 .*^

(/%) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ Unitless
  , unitsB ~ units1 :/: units2
  , unitsC ~ units2 :/: units1
  ) =>
  a ->
  b ->
  c
a /% b = Units.coerce (a ./. b)

infixl 7 /%

(!/) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1
  , unitsB ~ units1 :/: units2
  , unitsC ~ units2
  ) =>
  a ->
  b ->
  c
a !/ b = Units.coerce (a ./. b)

infixl 7 !/

(.!/!) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1 :*: units2
  , unitsB ~ units2
  , unitsC ~ units1
  ) =>
  a ->
  b ->
  c
a .!/! b = Units.coerce (a ./. b)

infixl 7 .!/!

(!./!) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1 :*: units2
  , unitsB ~ units1
  , unitsC ~ units2
  ) =>
  a ->
  b ->
  c
a !./! b = Units.coerce (a ./. b)

infixl 7 !./!

(!/!.) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1
  , unitsB ~ units1 :*: units2
  , unitsC ~ Unitless :/: units2
  ) =>
  a ->
  b ->
  c
a !/!. b = Units.coerce (a ./. b)

infixl 7 !/!.

(!/.!) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units2
  , unitsB ~ units1 :*: units2
  , unitsC ~ Unitless :/: units1
  ) =>
  a ->
  b ->
  c
a !/.! b = Units.coerce (a ./. b)

infixl 7 !/.!

(.!/.!) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1 :*: units3
  , unitsB ~ units2 :*: units3
  , unitsC ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .!/.! b = Units.coerce (a ./. b)

infixl 7 .!/.!

(./^) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1
  , unitsB ~ Unitless :/: units2
  , unitsC ~ units1 :*: units2
  ) =>
  a ->
  b ->
  c
a ./^ b = Units.coerce (a ./. b)

infixl 7 ./^

(!?/.!?) ::
  ( Division' a b c'
  , Units.Coercion c' c
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , HasUnits c unitsC erasedC
  , unitsA ~ units1 :*: units2
  , unitsB ~ units1 :*: units2 :*: units3
  , unitsC ~ Unitless :/: units3
  ) =>
  a ->
  b ->
  c
a !?/.!? b = Units.coerce (a ./. b)

infixl 7 !?/.!?
