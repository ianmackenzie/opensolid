module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , constant
  , new
  , on
  , derivative
  , point
  , range
  , bounds
  , reverse
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Plane3D, Point3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type Compiled space = Curve.Compiled 3 Meters space

constant :: Point3D space -> Curve3D space
new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
on :: Plane3D space -> Curve2D Meters -> Curve3D space
derivative :: Curve3D space -> VectorCurve3D Meters space
point :: Curve3D space -> Number -> Point3D space
range :: Curve3D space -> Interval Unitless -> Bounds3D space
bounds :: Curve3D space -> Bounds3D space
reverse :: Curve3D space -> Curve3D space
