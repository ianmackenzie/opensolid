module OpenSolid.Direction
  ( Direction
  , DirectionExists
  , unsafe
  , unwrap
  , areParallel
  , areIndependent
  , arePerpendicular
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

{-# INLINE areParallel #-}
areParallel ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
areParallel = Primitives.Abstract.directionAreParallel

{-# INLINE areIndependent #-}
areIndependent ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
areIndependent = Primitives.Abstract.directionAreIndependent

{-# INLINE arePerpendicular #-}
arePerpendicular ::
  DirectionExists dimension space =>
  Direction dimension space ->
  Direction dimension space ->
  Bool
arePerpendicular = Primitives.Abstract.directionArePerpendicular
