module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d
  , Compiled
  , derivative
  , new
  , xy
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

type role SurfaceFunction2d nominal nominal

type SurfaceFunction2d :: Type -> Type -> Type
data SurfaceFunction2d units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Point2d units space)
    UvBounds
    (Bounds2d units space)

instance HasField "compiled" (SurfaceFunction2d units space) (Compiled units space)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction2d units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d units1 space1)
    (SurfaceFunction2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (SurfaceFunction2d units space)
    (Curve2d units space)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d unitless uvSpace)
    (SurfaceFunction units)
    (SurfaceFunction units)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d unitless uvSpace)
    (VectorSurfaceFunction2d units space)
    (VectorSurfaceFunction2d units space)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d unitless uvSpace)
    (VectorSurfaceFunction3d units space)
    (VectorSurfaceFunction3d units space)

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2d units space) ->
  SurfaceFunction2d units space
xy :: SurfaceFunction units -> SurfaceFunction units -> SurfaceFunction2d units space
derivative ::
  SurfaceParameter ->
  SurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space
