module OpenSolid.Vector
  ( Vector
  , VectorExists
  , zero
  , squaredMagnitude_
  , magnitude
  , crossProductMagnitude_
  , componentIn
  , projectionIn
  , direction
  , magnitudeAndDirection
  , sum
  , transformBy
  , erase
  , unerase
  , coerce
  )
where

import Data.Coerce qualified
import OpenSolid.Error (IsZero)
import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract
  ( Direction
  , DirectionExists
  , Vector
  , VectorExists
  , VectorTransform
  )
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract

zero :: VectorExists dimension units space => Vector dimension units space
zero = Primitives.Abstract.vectorZero

squaredMagnitude_ ::
  VectorExists dimension units space =>
  Vector dimension units space ->
  Quantity (units ?*? units)
squaredMagnitude_ = Primitives.Abstract.vectorSquaredMagnitude_

magnitude :: VectorExists dimension units space => Vector dimension units space -> Quantity units
magnitude = Primitives.Abstract.vectorMagnitude

direction ::
  (VectorExists dimension units space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Direction dimension space)
direction = Primitives.Abstract.vectorDirection

magnitudeAndDirection ::
  (VectorExists dimension units space, DirectionExists dimension space, Tolerance units) =>
  Vector dimension units space ->
  Result IsZero (Quantity units, Direction dimension space)
magnitudeAndDirection = Primitives.Abstract.vectorMagnitudeAndDirection

crossProductMagnitude_ ::
  VectorExists dimension units space =>
  Vector dimension units space ->
  Vector dimension units space ->
  Quantity (units ?*? units)
crossProductMagnitude_ = Primitives.Abstract.vectorCrossProductMagnitude_

componentIn ::
  (VectorExists dimension units space, DirectionExists dimension space) =>
  Direction dimension space ->
  Vector dimension units space ->
  Quantity units
componentIn = Primitives.Abstract.vectorComponentIn

projectionIn ::
  (VectorExists dimension units space, DirectionExists dimension space) =>
  Direction dimension space ->
  Vector dimension units space ->
  Vector dimension units space
projectionIn = Primitives.Abstract.vectorProjectionIn

transformBy ::
  VectorExists dimension units space =>
  VectorTransform dimension tag space ->
  Vector dimension units space ->
  Vector dimension units space
transformBy = Primitives.Abstract.vectorTransformBy

sum ::
  VectorExists dimension units space =>
  List (Vector dimension units space) ->
  Vector dimension units space
sum = Primitives.Abstract.vectorSum

{-# INLINE erase #-}
erase ::
  VectorExists dimension units space =>
  Vector dimension units space ->
  Vector dimension Unitless space
erase = coerce

{-# INLINE unerase #-}
unerase ::
  forall units space dimension.
  VectorExists dimension units space =>
  Vector dimension Unitless space ->
  Vector dimension units space
unerase = coerce

{-# INLINE coerce #-}
coerce ::
  (VectorExists dimension units1 space, VectorExists dimension units2 space) =>
  Vector dimension units1 space ->
  Vector dimension units2 space
coerce = Data.Coerce.coerce
