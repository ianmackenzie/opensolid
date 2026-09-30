module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , IsDegenerate
  , new
  , derivative
  , bounds
  , reverse
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type Compiled space = Curve.Compiled 3 Meters space

type IsDegenerate space = Curve.IsDegenerate 3 Meters space

new ::
  Tolerance Meters =>
  Compiled space ->
  VectorCurve3D Meters space ->
  Result (IsDegenerate space) (Curve3D space)
derivative :: Curve3D space -> VectorCurve3D Meters space
bounds :: Curve3D space -> Bounds3D space
reverse :: Curve3D space -> Curve3D space
