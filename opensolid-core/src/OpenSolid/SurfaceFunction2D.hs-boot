module OpenSolid.SurfaceFunction2D
  ( SurfaceFunction2D
  , Compiled
  , derivative
  , new
  , xy
  )
where

import GHC.Records (HasField)
import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D, Point2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvSpace (UvSpace)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

type role SurfaceFunction2D nominal nominal

type SurfaceFunction2D :: Type -> Type -> Type
data SurfaceFunction2D units space

type Compiled units space =
  CompiledFunction
    UvPoint
    (Point2D units space)
    UvBounds
    (Bounds2D units space)

instance HasField "compiled" (SurfaceFunction2D units space) (Compiled units space)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2D units1 space1)
    (SurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2D units space)
    (Curve2D unitless uvSpace)
    (Curve2D units space)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction1D units)
    (SurfaceFunction2D unitless uvSpace)
    (SurfaceFunction1D units)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (VectorSurfaceFunction2D units space)
    (SurfaceFunction2D unitless uvSpace)
    (VectorSurfaceFunction2D units space)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (VectorSurfaceFunction3D units space)
    (SurfaceFunction2D unitless uvSpace)
    (VectorSurfaceFunction3D units space)

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2D units space) ->
  SurfaceFunction2D units space
xy :: SurfaceFunction1D units -> SurfaceFunction1D units -> SurfaceFunction2D units space
derivative ::
  SurfaceParameter ->
  SurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
