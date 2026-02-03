module OpenSolid.Vector
  ( Vector
  , Exists
  , IsZero (IsZero)
  , zero
  , squaredMagnitude_
  , squaredMagnitude
  , magnitude
  , componentIn
  , projectionIn
  , normalize
  , direction
  , magnitudeAndDirection
  , sum
  , erase
  , unerase
  )
where

import Data.Void (Void)
import {-# SOURCE #-} OpenSolid.Direction (Direction)
import {-# SOURCE #-} OpenSolid.Direction qualified as Direction
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.Vector2D (Vector2D)
import {-# SOURCE #-} OpenSolid.Vector3D (Vector3D)

type family Vector dimension units space = vector | vector -> dimension units space where
  Vector 1 units Void = Quantity units
  Vector 2 units space = Vector2D units space
  Vector 3 units space = Vector3D units space

class
  ( HasZero (Vector dimension units space)
  , HasUnits (Vector dimension units space) units
  , Units.Coercion (Vector dimension units space) (Vector dimension Unitless space)
  , Units.Coercion (Vector dimension Unitless space) (Vector dimension units space)
  , Eq (Vector dimension units space)
  , ApproximateEquality (Vector dimension units space) units
  , Negation (Vector dimension units space)
  , Addition
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Addition
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Subtraction
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Multiplication Number (Vector dimension units space) (Vector dimension units space)
  , Multiplication (Vector dimension units space) Number (Vector dimension units space)
  , Multiplication (Quantity units) (Vector dimension Unitless space) (Vector dimension units space)
  , Multiplication (Vector dimension Unitless space) (Quantity units) (Vector dimension units space)
  , Division (Vector dimension units space) Number (Vector dimension units space)
  , Division (Vector dimension units space) (Quantity units) (Vector dimension Unitless space)
  , DotMultiplication (Vector dimension units space) (Vector dimension Unitless space) (Quantity units)
  , Exists dimension Unitless space
  ) =>
  Exists dimension units space
  where
  zero :: Vector dimension units space
  zero = HasZero.zero

  squaredMagnitude_ :: Vector dimension units space -> Quantity (units ?*? units)

data IsZero = IsZero deriving (Eq, Show)

instance Exists 1 units Void where
  {-# INLINEABLE squaredMagnitude_ #-}
  squaredMagnitude_ = Quantity.squared_

instance Exists 2 units space where
  {-# INLINEABLE squaredMagnitude_ #-}
  squaredMagnitude_ vector = vector `dot_` vector

instance Exists 3 units space where
  {-# INLINEABLE squaredMagnitude_ #-}
  squaredMagnitude_ vector = vector `dot_` vector

{-# INLINEABLE squaredMagnitude #-}
squaredMagnitude ::
  (Exists dimension units space, Units.Squared units squaredUnits) =>
  Vector dimension units space ->
  Quantity squaredUnits
squaredMagnitude = Units.specialize . squaredMagnitude_

{-# INLINEABLE magnitude #-}
magnitude :: Exists dimension units space => Vector dimension units space -> Quantity units
magnitude = Quantity.sqrt_ . squaredMagnitude_

normalize ::
  (Exists dimension units space, Tolerance units) =>
  Vector dimension units space ->
  Vector dimension Unitless space
normalize vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then zero
    else vector ./. vectorMagnitude

direction ::
  (Exists dimension units space, Direction.Exists dimension space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Direction dimension space)
direction vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then Error IsZero
    else Ok (Direction.unsafe (vector ./. vectorMagnitude))

magnitudeAndDirection ::
  (Exists dimension units space, Direction.Exists dimension space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Quantity units, Direction dimension space)
magnitudeAndDirection vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then Error IsZero
    else Ok (vectorMagnitude, Direction.unsafe (vector ./. vectorMagnitude))

{-# INLINEABLE componentIn #-}
componentIn ::
  (Exists dimension units space, Direction.Exists dimension space) =>
  Direction dimension space ->
  Vector dimension units space ->
  Quantity units
componentIn givenDirection vector = Direction.unwrap givenDirection `dot` vector

{-# INLINEABLE projectionIn #-}
projectionIn ::
  (Exists dimension units space, Direction.Exists dimension space) =>
  Direction dimension space ->
  Vector dimension units space ->
  Vector dimension units space
projectionIn givenDirection vector =
  Direction.unwrap givenDirection .*. componentIn givenDirection vector

sum ::
  Exists dimension units space =>
  List (Vector dimension units space) ->
  Vector dimension units space
sum = List.foldl (.+.) zero

{-# INLINE erase #-}
erase ::
  Exists dimension units space =>
  Vector dimension units space ->
  Vector dimension Unitless space
erase = Units.coerce

{-# INLINE unerase #-}
unerase ::
  Exists dimension units space =>
  Vector dimension Unitless space ->
  Vector dimension units space
unerase = Units.coerce
