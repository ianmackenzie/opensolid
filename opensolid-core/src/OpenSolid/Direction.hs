module OpenSolid.Direction
  ( Direction
  , DirectionExists
  , unsafe
  , unwrap
  , parallel
  , independent
  , perpendicular
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (Direction, DirectionExists, Vector)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract

{-# INLINE unsafe #-}
unsafe ::
  DirectionExists dimension space =>
  Vector dimension Unitless space ->
  Direction dimension space
unsafe = Primitives.Abstract.directionUnsafe

{-# INLINE unwrap #-}
unwrap ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Vector dimension Unitless space
unwrap = Primitives.Abstract.directionUnwrap

{-# INLINE parallel #-}
parallel ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
parallel = Primitives.Abstract.directionParallel

{-# INLINE independent #-}
independent ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
independent = Primitives.Abstract.directionIndependent

{-# INLINE perpendicular #-}
perpendicular ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
perpendicular = Primitives.Abstract.directionPerpendicular
