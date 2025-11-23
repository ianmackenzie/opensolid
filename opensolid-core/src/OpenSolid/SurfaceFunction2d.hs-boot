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
data SurfaceFunction2d space units

type Compiled space units =
  CompiledFunction
    UvPoint
    (Point2d space units)
    UvBounds
    (Bounds2d space units)

instance HasField "compiled" (SurfaceFunction2d space units) (Compiled space units)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)
    (SurfaceFunction2d space1 units1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d space1 units1)
    (SurfaceFunction2d space2 units2)
    (VectorSurfaceFunction2d space1 units1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d uvSpace unitless)
    (SurfaceFunction2d space units)
    (Curve2d space units)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (SurfaceFunction units)
    (SurfaceFunction units)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (VectorSurfaceFunction2d space units)
    (VectorSurfaceFunction2d space units)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (VectorSurfaceFunction3d space units)
    (VectorSurfaceFunction3d space units)

new ::
  Compiled space units ->
  (SurfaceParameter -> VectorSurfaceFunction2d space units) ->
  SurfaceFunction2d space units
xy :: SurfaceFunction units -> SurfaceFunction units -> SurfaceFunction2d space units
derivative ::
  SurfaceParameter ->
  SurfaceFunction2d space units ->
  VectorSurfaceFunction2d space units
