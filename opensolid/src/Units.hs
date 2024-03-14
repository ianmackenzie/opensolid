module Units
  ( Conversion
  , conversion
  , convert
  , unconvert
  , Coercion
  , (:*:)
  , (:/:)
  , drop
  , add
  , specialize
  , unspecialize
  , (.*.)
  , (./.)
  , (.<>.)
  , (.><.)
  , (^*.)
  , (.*^)
  , (.!/!)
  , (!./!)
  , (.!/.!)
  , (./^)
  , (!?/.!?)
  , Product
  , Squared
  , Quotient
  , Unitless
  , Radians
  , Meters
  , Seconds
  , MetersPerSecond
  , MetersPerSecondSquared
  , SquareMeters
  , CubicMeters
  )
where

import Arithmetic
import Basics
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import {-# SOURCE #-} Qty (Qty (Qty))
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified

newtype Conversion units1 units2 = Conversion Prelude.Double

conversion :: Qty units1 -> Qty units2 -> Conversion units1 units2
conversion (Qty a) (Qty b) = Conversion (b Prelude./ a)

convert :: Conversion units1 units2 -> Qty units1 -> Qty units2
convert (Conversion factor) (Qty value) = Qty (value Prelude.* factor)

unconvert :: Conversion units1 units2 -> Qty units2 -> Qty units1
unconvert (Conversion factor) (Qty value) = Qty (value Prelude./ factor)

type Coercion :: Type -> Type -> Type -> Type -> Constraint
class Coercion u1 u2 a b | a -> u1, b -> u2, a u2 -> b

instance Coercion u1 u2 a b => Coercion u1 u2 (List a) (List b)

instance Coercion u1 u2 a b => Coercion u1 u2 (NonEmpty a) (NonEmpty b)

data units1 :*: units2

data units1 :/: units2

infixl 7 :*:, :/:

{-# INLINE drop #-}
drop :: Coercion units Unitless a b => a -> b
drop = unsafeCoerce

{-# INLINE add #-}
add :: Coercion Unitless units a b => a -> b
add = unsafeCoerce

{-# INLINE specialize #-}
specialize :: (Coercion genericUnits specificUnits a b, Specialize genericUnits specificUnits) => a -> b
specialize = unsafeCoerce

{-# INLINE unspecialize #-}
unspecialize :: (Coercion genericUnits specificUnits a b, Specialize genericUnits specificUnits) => b -> a
unspecialize = unsafeCoerce

type SimplifyProduct unitsA unitsB unitsC aUnitless bUnitless cUnitless a b c =
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , Multiplication aUnitless bUnitless cUnitless
  )

type SimplifyQuotient unitsA unitsB unitsC aUnitless bUnitless cUnitless a b c =
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , Division aUnitless bUnitless cUnitless
  )

(.*.) ::
  SimplifyProduct units1 units2 (units1 :*: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(.*.) lhs rhs = add (drop lhs * drop rhs)

(.<>.) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless (unitsA :*: unitsB) cUnitless c
  , DotProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.<>.) lhs rhs = add (drop lhs <> drop rhs)

(.><.) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless (unitsA :*: unitsB) cUnitless c
  , CrossProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.><.) lhs rhs = add (drop lhs >< drop rhs)

(./.) ::
  SimplifyQuotient units1 units2 (units1 :/: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(./.) lhs rhs = add (drop lhs / drop rhs)

(^*.) ::
  SimplifyProduct units1 (Unitless :/: units2) (units1 :/: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(^*.) lhs rhs = add (drop lhs * drop rhs)

(.*^) ::
  SimplifyProduct (Unitless :/: units2) units1 (units1 :/: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(.*^) lhs rhs = add (drop lhs * drop rhs)

(.!/!) ::
  SimplifyQuotient (units1 :*: units2) units2 units1 aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(.!/!) lhs rhs = add (drop lhs / drop rhs)

(!./!) ::
  SimplifyQuotient (units1 :*: units2) units1 units2 aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(!./!) lhs rhs = add (drop lhs / drop rhs)

(.!/.!) ::
  SimplifyQuotient (units1 :*: units2) (units2 :*: units2) (units1 :/: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(.!/.!) lhs rhs = add (drop lhs / drop rhs)

(./^) ::
  SimplifyQuotient units1 (Unitless :/: units2) (units1 :*: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(./^) lhs rhs = add (drop lhs / drop rhs)

(!?/.!?) ::
  SimplifyQuotient (units1 :*: units2) (units1 :*: units2 :*: units3) (Unitless :/: units3) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(!?/.!?) lhs rhs = add (drop lhs / drop rhs)

infixl 7 .*., ./., .<>., .><.

infixl 7 ^*., .*^, .!/!, !./!, .!/.!, ./^, !?/.!?

class
  ( Product units2 units1 units3
  , Quotient units3 units1 units2
  , Quotient units3 units2 units1
  ) =>
  Product units1 units2 units3
    | units1 units2 -> units3

class
  ( Product units3 units2 units1
  , Product units2 units3 units1
  , Quotient units1 units3 units2
  ) =>
  Quotient units1 units2 units3
    | units1 units2 -> units3

class
  Product units units squaredUnits =>
  Squared units squaredUnits
    | units -> squaredUnits
    , squaredUnits -> units

data Unitless

data Radians

data Meters

data Seconds

data MetersPerSecond

data MetersPerSecondSquared

data SquareMeters

data CubicMeters

instance {-# INCOHERENT #-} Product Unitless Unitless Unitless

instance {-# INCOHERENT #-} Product Unitless units units

instance {-# INCOHERENT #-} Product units Unitless units

instance {-# INCOHERENT #-} Quotient Unitless Unitless Unitless

instance {-# INCOHERENT #-} Quotient units Unitless units

instance {-# INCOHERENT #-} Quotient units units Unitless

instance Product Meters Meters SquareMeters

instance Product Meters SquareMeters CubicMeters

instance Product SquareMeters Meters CubicMeters

instance Product Seconds MetersPerSecond Meters

instance Product MetersPerSecond Seconds Meters

instance Product Seconds MetersPerSecondSquared MetersPerSecond

instance Product MetersPerSecondSquared Seconds MetersPerSecond

instance Quotient SquareMeters Meters Meters

instance Quotient CubicMeters Meters SquareMeters

instance Quotient CubicMeters SquareMeters Meters

instance Quotient Meters Seconds MetersPerSecond

instance Quotient Meters MetersPerSecond Seconds

instance Quotient MetersPerSecond Seconds MetersPerSecondSquared

instance Quotient MetersPerSecond MetersPerSecondSquared Seconds

instance Squared Unitless Unitless

instance Squared Meters SquareMeters

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Product units1 units2 units3 => Specialize (units1 :*: units2) units3

instance Quotient units1 units2 units3 => Specialize (units1 :/: units2) units3
