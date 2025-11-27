module OpenSolid.Curve3d
  ( Curve3d
  , Compiled
  , constant
  , new
  , on
  , evaluate
  , evaluateBounds
  , reverse
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)

type role Curve3d nominal

type Curve3d :: Type -> Type
data Curve3d space

type Compiled space =
  CompiledFunction Number (Point3d space) (Bounds Unitless) (Bounds3d space)

instance HasField "compiled" (Curve3d space) (Compiled space)

instance HasField "derivative" (Curve3d space) (VectorCurve3d Meters space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3d space1)
    (VectorCurve3d meters space2)
    (Curve3d space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3d space1)
    (VectorCurve3d meters space2)
    (Curve3d space1)

constant :: Point3d space -> Curve3d space
new :: Compiled space -> VectorCurve3d Meters space -> Curve3d space
on :: Plane3d global local -> Curve2d Meters local -> Curve3d global
evaluate :: Curve3d space -> Number -> Point3d space
evaluateBounds :: Curve3d space -> Bounds Unitless -> Bounds3d space
reverse :: Curve3d space -> Curve3d space
