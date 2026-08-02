module OpenSolid.VectorBounds
  ( VectorBounds
  , VectorBoundsExists
  , member
  , center
  , squaredMagnitude_
  , magnitude
  , normalize
  , direction
  , diameter
  , isResolved
  , areDistinct
  , areIndependent
  , transformBy
  , erase
  , unerase
  , coerce
  )
where

import Data.Coerce qualified
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (DirectionBounds, VectorBounds, VectorBoundsExists, VectorTransform)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract
import OpenSolid.Vector (Vector)

{-# INLINE member #-}
member ::
  VectorBoundsExists dimension units space =>
  Vector dimension units space ->
  VectorBounds dimension units space ->
  Bool
member = Primitives.Abstract.vectorBoundsMember

{-# INLINE center #-}
center ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  Vector dimension units space
center = Primitives.Abstract.vectorBoundsCenter

{-# INLINE squaredMagnitude_ #-}
squaredMagnitude_ ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  Interval (units ?*? units)
squaredMagnitude_ = Primitives.Abstract.vectorBoundsSquaredMagnitude_

{-# INLINE magnitude #-}
magnitude ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  Interval units
magnitude = Primitives.Abstract.vectorBoundsMagnitude

{-# INLINE direction #-}
direction ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  DirectionBounds dimension space
direction = Primitives.Abstract.vectorBoundsDirection

{-# INLINE normalize #-}
normalize ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension Unitless space
normalize = Primitives.Abstract.vectorBoundsNormalize

{-# INLINE diameter #-}
diameter ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  Quantity units
diameter = Primitives.Abstract.vectorBoundsDiameter

{-# INLINE isResolved #-}
isResolved ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  Bool
isResolved = Primitives.Abstract.vectorBoundsIsResolved

{-# INLINE areDistinct #-}
areDistinct ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  Bool
areDistinct = Primitives.Abstract.vectorBoundsAreDistinct

{-# INLINE areIndependent #-}
areIndependent ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  Bool
areIndependent = Primitives.Abstract.vectorBoundsAreIndependent

{-# INLINE transformBy #-}
transformBy ::
  VectorBoundsExists dimension units space =>
  VectorTransform dimension tag space ->
  VectorBounds dimension units space ->
  VectorBounds dimension units space
transformBy = Primitives.Abstract.vectorBoundsTransformBy

{-# INLINE erase #-}
erase ::
  VectorBoundsExists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension Unitless space
erase = coerce

{-# INLINE unerase #-}
unerase ::
  forall units space dimension.
  VectorBoundsExists dimension units space =>
  VectorBounds dimension Unitless space ->
  VectorBounds dimension units space
unerase = coerce

{-# INLINE coerce #-}
coerce ::
  (VectorBoundsExists dimension units1 space, VectorBoundsExists dimension units2 space) =>
  VectorBounds dimension units1 space ->
  VectorBounds dimension units2 space
coerce = Data.Coerce.coerce
