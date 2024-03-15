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
  , Specialize
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
  , (!/!.)
  , (!/.!)
  , (.!/.!)
  , (./^)
  , (!?/.!?)
  , leftAssociate
  , rightAssociate
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

(!/!.) ::
  SimplifyQuotient units1 (units1 :*: units2) (Unitless :/: units2) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(!/!.) lhs rhs = add (drop lhs / drop rhs)

(!/.!) ::
  SimplifyQuotient units2 (units1 :*: units2) (Unitless :/: units1) aUnitless bUnitless cUnitless a b c =>
  a ->
  b ->
  c
(!/.!) lhs rhs = add (drop lhs / drop rhs)

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

leftAssociate :: Coercion (units1 :*: (units2 :*: units3)) ((units1 :*: units2) :*: units3) a b => a -> b
leftAssociate = unsafeCoerce

rightAssociate :: Coercion ((units1 :*: units2) :*: units3) (units1 :*: (units2 :*: units3)) a b => a -> b
rightAssociate = unsafeCoerce

data Unitless

data Radians

data Meters

data Seconds

data MetersPerSecond

data MetersPerSecondSquared

data SquareMeters

data CubicMeters

type Prod :: Type -> Type -> Type
type family Prod units1 units2

type Quot :: Type -> Type -> Type
type family Quot units1 units2

type Sqr :: Type -> Type
type family Sqr units = squaredUnits | squaredUnits -> units

type instance Prod Unitless Unitless = Unitless

type instance Prod Unitless units = units

type instance Prod units Unitless = units

type instance Quot Unitless Unitless = Unitless

type instance Quot units Unitless = units

type instance Quot units units = Unitless

type instance Prod Meters Meters = SquareMeters

type instance Prod Meters SquareMeters = CubicMeters

type instance Prod SquareMeters Meters = CubicMeters

type instance Prod Seconds MetersPerSecond = Meters

type instance Prod MetersPerSecond Seconds = Meters

type instance Prod Seconds MetersPerSecondSquared = MetersPerSecond

type instance Prod MetersPerSecondSquared Seconds = MetersPerSecond

type instance Quot SquareMeters Meters = Meters

type instance Quot CubicMeters Meters = SquareMeters

type instance Quot CubicMeters SquareMeters = Meters

type instance Quot Meters Seconds = MetersPerSecond

type instance Quot Meters MetersPerSecond = Seconds

type instance Quot MetersPerSecond Seconds = MetersPerSecondSquared

type instance Quot MetersPerSecond MetersPerSecondSquared = Seconds

type instance Sqr Unitless = Unitless

type instance Sqr Meters = SquareMeters

type Product units1 units2 units3 =
  ( Prod units1 units2 ~ units3
  , Prod units2 units1 ~ units3
  , Quot units3 units1 ~ units2
  , Quot units3 units2 ~ units1
  )

type Quotient units1 units2 units3 =
  ( Quot units1 units2 ~ units3
  , Product units2 units3 units1
  )

type Squared units1 units2 =
  ( Sqr units1 ~ units2
  , Product units1 units1 units2
  )

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Product units1 units2 units3 => Specialize (units1 :*: units2) units3

instance Quotient units1 units2 units3 => Specialize (units1 :/: units2) units3

instance Specialize Unitless Unitless
