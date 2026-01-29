module OpenSolid.Vector
  ( Vector
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
  )
where

import OpenSolid.CoordinateSystem (Direction, Vector)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Direction qualified as Direction
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units

data IsZero = IsZero deriving (Eq, Show)

{-# INLINEABLE zero #-}
zero :: CoordinateSystem.Generic dimension units space => Vector dimension units space
zero = HasZero.zero

{-# INLINEABLE squaredMagnitude_ #-}
squaredMagnitude_ ::
  CoordinateSystem.Vectorial dimension units space =>
  Vector dimension units space ->
  Quantity (units ?*? units)
squaredMagnitude_ vector = vector `dot_` vector

{-# INLINEABLE squaredMagnitude #-}
squaredMagnitude ::
  (CoordinateSystem.Vectorial dimension units1 space, Units.Squared units1 units2) =>
  Vector dimension units1 space ->
  Quantity units2
squaredMagnitude = Units.specialize . squaredMagnitude_

{-# INLINEABLE magnitude #-}
magnitude ::
  CoordinateSystem.Vectorial dimension units space =>
  Vector dimension units space ->
  Quantity units
magnitude = Quantity.sqrt_ . squaredMagnitude_

normalize ::
  forall dimension units space.
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  Vector dimension units space ->
  Vector dimension Unitless space
normalize vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then HasZero.zero
    else vector ./. vectorMagnitude

direction ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Direction dimension space)
direction vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then Error IsZero
    else Ok (Direction.unsafe (vector ./. vectorMagnitude))

magnitudeAndDirection ::
  (CoordinateSystem.Vectorial dimension units space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Quantity units, Direction dimension space)
magnitudeAndDirection vector = do
  let vectorMagnitude = magnitude vector
  if vectorMagnitude ~= Quantity.zero
    then Error IsZero
    else do
      let vectorDirection = Direction.unsafe (vector ./. vectorMagnitude)
      Ok (vectorMagnitude, vectorDirection)

{-# INLINEABLE componentIn #-}
componentIn ::
  CoordinateSystem.Vectorial dimension units space =>
  Direction dimension space ->
  Vector dimension units space ->
  Quantity units
componentIn = dot

projectionIn ::
  CoordinateSystem.Vectorial dimension units space =>
  Direction dimension space ->
  Vector dimension units space ->
  Vector dimension units space
projectionIn givenDirection vector =
  givenDirection .*. componentIn givenDirection vector

sum ::
  CoordinateSystem.Generic dimension units space =>
  List (Vector dimension units space) ->
  Vector dimension units space
sum = List.foldl (.+.) zero
