module OpenSolid.Functions
  ( Curve (..)
  , newCurve
  , recursiveCurve
  , curveValue
  , curveBounds
  , Curve2d (..)
  -- , Curve3d (..)
  , VectorCurve2d (..)
  -- , VectorCurve3d (..)
  , SurfaceFunction (..)
  , SurfaceFunction2d (..)
  -- , SurfaceFunction3d (..)
  , VectorSurfaceFunction2d (..)
  -- , VectorSurfaceFunciton3d (..)
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

data Curve units where
  Curve ::
    { compiled :: CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)
    , derivative :: ~(Curve units)
    , composeCurve :: Curve Unitless -> Curve units
    , composeSurfaceFunction :: SurfaceFunction Unitless -> SurfaceFunction units
    } ->
    Curve units

instance HasUnits (Curve units) units

instance Units.Coercion (Curve units1) (Curve units2) where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , composeCurve = Units.coerce . curve.composeCurve
      , composeSurfaceFunction = Units.coerce . curve.composeSurfaceFunction
      }

instance FFI (Curve Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Qty units2) units1
  where
  curve ~= value = List.allTrue [curveValue curve tValue ~= value | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Curve units2) units1
  where
  curve1 ~= curve2 =
    List.allTrue
      [ curveValue curve1 tValue ~= curveValue curve2 tValue
      | tValue <- Parameter.samples
      ]

newCurve ::
  ( "compiled" ::: CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)
  , "derivative" ::: Curve units
  , "composeCurve" ::: (Curve Unitless -> Curve units)
  , "composeSurfaceFunction" ::: (SurfaceFunction Unitless -> SurfaceFunction units)
  ) ->
  Curve units
newCurve args =
  Curve
    { compiled = args.compiled
    , derivative = args.derivative
    , composeCurve = args.composeCurve
    , composeSurfaceFunction = args.composeSurfaceFunction
    }

recursiveCurve ::
  ( Curve units ->
    ( "compiled" ::: CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)
    , "derivative" ::: Curve units
    , "composeCurve" ::: (Curve Unitless -> Curve units)
    , "composeSurfaceFunction" ::: (SurfaceFunction Unitless -> SurfaceFunction units)
    )
  ) ->
  Curve units
recursiveCurve callback = let result = newCurve (callback result) in result

{-# INLINE curveValue #-}
curveValue :: Curve units -> Float -> Qty units
curveValue curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE curveBounds #-}
curveBounds :: Curve units -> Bounds Unitless -> Bounds units
curveBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

-- | A parametric curve in 2D space.
data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d ::
    CompiledFunction
      Float
      (Point2d (space @ units))
      (Bounds Unitless)
      (Bounds2d (space @ units)) ->
    ~(VectorCurve2d (space @ units)) ->
    Curve2d (space @ units)

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    CompiledFunction
      Float
      (Vector2d (space @ units))
      (Bounds Unitless)
      (VectorBounds2d (space @ units)) ->
    ~(VectorCurve2d (space @ units)) ->
    VectorCurve2d (space @ units)

data SurfaceFunction units where
  SurfaceFunction ::
    CompiledFunction UvPoint (Qty units) UvBounds (Bounds units) ->
    ~(SurfaceFunction units) ->
    ~(SurfaceFunction units) ->
    SurfaceFunction units

instance HasUnits (SurfaceFunction units) units

instance Units.Coercion (SurfaceFunction unitsA) (SurfaceFunction unitsB) where
  coerce (SurfaceFunction c du dv) =
    SurfaceFunction (Units.coerce c) (Units.coerce du) (Units.coerce dv)

data SurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction2d ::
    CompiledFunction
      UvPoint
      (Point2d (space @ units))
      UvBounds
      (Bounds2d (space @ units)) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    SurfaceFunction2d (space @ units)

data VectorSurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction2d ::
    CompiledFunction
      UvPoint
      (Vector2d (space @ units))
      UvBounds
      (VectorBounds2d (space @ units)) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    VectorSurfaceFunction2d (space @ units)
