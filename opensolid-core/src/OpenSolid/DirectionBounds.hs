module OpenSolid.DirectionBounds
  ( DirectionBounds
  , DirectionBoundsExists
  , unsafe
  , unwrap
  , areDistinct
  , areIndependent
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (DirectionBounds, DirectionBoundsExists, VectorBounds)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract

{-# INLINE unsafe #-}
unsafe ::
  DirectionBoundsExists dimension space =>
  VectorBounds dimension Unitless space ->
  DirectionBounds dimension space
unsafe = Primitives.Abstract.directionBoundsUnsafe

{-# INLINE unwrap #-}
unwrap ::
  DirectionBoundsExists dimension space =>
  DirectionBounds dimension space ->
  VectorBounds dimension Unitless space
unwrap = Primitives.Abstract.directionBoundsUnwrap

{-# INLINE areDistinct #-}
areDistinct ::
  DirectionBoundsExists dimension space =>
  DirectionBounds dimension space ->
  DirectionBounds dimension space ->
  Bool
areDistinct = Primitives.Abstract.directionBoundsAreDistinct

areIndependent ::
  DirectionBoundsExists dimension space =>
  DirectionBounds dimension space ->
  DirectionBounds dimension space ->
  Bool
areIndependent = Primitives.Abstract.directionBoundsAreIndependent
