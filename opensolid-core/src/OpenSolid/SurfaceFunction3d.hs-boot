module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d
  , Compiled
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , transformBy
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units (HasUnits)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

type role SurfaceFunction3d nominal

type SurfaceFunction3d :: Type -> Type
data SurfaceFunction3d space

instance
  HasField
    "du"
    (SurfaceFunction3d space)
    (VectorSurfaceFunction3d space Meters)

instance
  HasField
    "dv"
    (SurfaceFunction3d space)
    (VectorSurfaceFunction3d space Meters)

type Compiled space =
  CompiledFunction
    UvPoint
    (Point3d space Meters)
    UvBounds
    (Bounds3d space Meters)

instance HasField "compiled" (SurfaceFunction3d space) (Compiled space)

instance HasUnits (SurfaceFunction3d space) Meters

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3d space1)
    (VectorSurfaceFunction3d space2 meters)
    (SurfaceFunction3d space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3d space1)
    (VectorSurfaceFunction3d space2 meters)
    (SurfaceFunction3d space1)

constant :: Point3d space Meters -> SurfaceFunction3d space
evaluate :: SurfaceFunction3d space -> UvPoint -> Point3d space Meters
evaluateBounds :: SurfaceFunction3d space -> UvBounds -> Bounds3d space Meters
derivative ::
  SurfaceParameter ->
  SurfaceFunction3d space ->
  VectorSurfaceFunction3d space Meters
transformBy ::
  Transform3d tag space Meters ->
  SurfaceFunction3d space ->
  SurfaceFunction3d space
