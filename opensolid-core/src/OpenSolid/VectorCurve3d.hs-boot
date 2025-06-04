module OpenSolid.VectorCurve3d
  ( VectorCurve3d (compiled, derivative)
  , Compiled
  , constant
  , new
  , on
  , evaluate
  , evaluateBounds
  , unsafeMagnitude
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

data VectorCurve3d (coordinateSystem :: CoordinateSystem) where
  VectorCurve3d ::
    { compiled :: Compiled (space @ units)
    , derivative :: ~(VectorCurve3d (space @ units))
    } ->
    VectorCurve3d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector3d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds3d coordinateSystem)

instance HasUnits (VectorCurve3d (space @ units)) units (VectorCurve3d (space @ Unitless))

instance Negation (VectorCurve3d (space @ units))

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units))

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ unitsA)) (VectorCurve3d (space2 @ unitsB))

instance
  Multiplication'
    (Curve units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))

instance
  Division'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :/: units2)))

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve units3)

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
new :: Compiled (space @ units) -> VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
on :: PlaneOrientation3d space (Defines local) -> VectorCurve2d (local @ units) -> VectorCurve3d (space @ units)
evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluateBounds :: VectorCurve3d (space @ units) -> Bounds Unitless -> VectorBounds3d (space @ units)
unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve units
transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
