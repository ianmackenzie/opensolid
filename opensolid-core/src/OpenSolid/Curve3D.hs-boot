module OpenSolid.Curve3D
  ( Curve3D
  , Compiled
  , constant
  , new
  , on
  , derivative
  , evaluate
  , evaluateBounds
  , bounds
  , reverse
  )
where

import GHC.Records (HasField)
import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Plane3D, Point3D, Vector3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type role Curve3D nominal

type Curve3D :: Type -> Type
data Curve3D space

type Compiled space =
  CompiledFunction Number (Point3D space) (Interval Unitless) (Bounds3D space)

instance HasField "compiled" (Curve3D space) (Compiled space)

instance HasField "derivative" (Curve3D space) (VectorCurve3D Meters space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3D space1)
    (VectorCurve3D meters space2)
    (Curve3D space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve3D space1)
    (Vector3D meters space2)
    (Curve3D space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3D space1)
    (VectorCurve3D meters space2)
    (Curve3D space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve3D space1)
    (Vector3D meters space2)
    (Curve3D space1)

instance
  space1 ~ space2 =>
  Subtraction
    (Curve3D space1)
    (Curve3D space2)
    (VectorCurve3D Meters space1)

instance
  space1 ~ space2 =>
  Subtraction
    (Curve3D space1)
    (Point3D space2)
    (VectorCurve3D Meters space1)

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (Curve3D space2)
    (VectorCurve3D Meters space1)

constant :: Point3D space -> Curve3D space
new :: Compiled space -> VectorCurve3D Meters space -> Curve3D space
on :: Plane3D global local -> Curve2D Meters local -> Curve3D global
derivative :: Curve3D space -> VectorCurve3D Meters space
evaluate :: Curve3D space -> Number -> Point3D space
evaluateBounds :: Curve3D space -> Interval Unitless -> Bounds3D space
bounds :: Curve3D space -> Bounds3D space
reverse :: Curve3D space -> Curve3D space
