module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , SearchTree
  , constant
  , new
  , on
  , derivative
  , point
  , bounds
  , overallBounds
  , reverse
  , searchTree
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.Search qualified as Curve.Search
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Plane3D, Point3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type Compiled space = Curve.Compiled 3 Meters space

type SearchTree space = Curve.Search.Tree 3 Meters space

constant :: Point3D space -> Curve3D space
new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
on :: Plane3D space -> Curve2D Meters -> Curve3D space
derivative :: Curve3D space -> VectorCurve3D Meters space
point :: Curve3D space -> Number -> Point3D space
bounds :: Curve3D space -> Interval Unitless -> Bounds3D space
overallBounds :: Curve3D space -> Bounds3D space
reverse :: Curve3D space -> Curve3D space
searchTree :: Curve3D space -> SearchTree space
