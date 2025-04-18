module OpenSolid.Expression
  ( Expression
  , Constant
  , zero
  , origin
  , constant
  , xy
  , xyz
  , xComponent
  , yComponent
  , zComponent
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , t
  , r
  , u
  , v
  , sqrt
  , sqrt'
  , squared
  , squared'
  , sin
  , cos
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , Evaluation (evaluate, evaluateBounds)
  )
where

import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import OpenSolid.Angle (Angle)
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds3d (Bounds3d (Bounds3d))
import OpenSolid.Expression.Scalar (Scalar)
import OpenSolid.Expression.Scalar qualified as Scalar
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.SurfaceParameter (UvBounds, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude (Double)

type role Expression nominal nominal

data Expression input output where
  Curve1d ::
    { c1x :: Scalar Float
    , c1v :: ~Curve1dValueFunction
    , c1b :: ~Curve1dBoundsFunction
    } ->
    Expression Float (Qty units)
  Surface1d ::
    { s1x :: Scalar UvPoint
    , s1v :: ~Surface1dValueFunction
    , s1b :: ~Surface1dBoundsFunction
    } ->
    Expression UvPoint (Qty units)
  Curve2d ::
    { c2x :: Scalar Float
    , c2y :: Scalar Float
    , c2v :: ~Curve2dValueFunction
    , c2b :: ~Curve2dBoundsFunction
    } ->
    Expression Float (Point2d (space @ units))
  Surface2d ::
    { s2x :: Scalar UvPoint
    , s2y :: Scalar UvPoint
    , s2v :: ~Surface2dValueFunction
    , s2b :: ~Surface2dBoundsFunction
    } ->
    Expression UvPoint (Point2d (space @ units))
  VectorCurve2d ::
    { vc2x :: Scalar Float
    , vc2y :: Scalar Float
    , vc2v :: ~Curve2dValueFunction
    , vc2b :: ~Curve2dBoundsFunction
    } ->
    Expression Float (Vector2d (space @ units))
  VectorSurface2d ::
    { vs2x :: Scalar UvPoint
    , vs2y :: Scalar UvPoint
    , vs2v :: ~Surface2dValueFunction
    , vs2b :: ~Surface2dBoundsFunction
    } ->
    Expression UvPoint (Vector2d (space @ units))
  Curve3d ::
    { c3x :: Scalar Float
    , c3y :: Scalar Float
    , c3z :: Scalar Float
    , c3v :: ~Curve3dValueFunction
    , c3b :: ~Curve3dBoundsFunction
    } ->
    Expression Float (Point3d (space @ units))
  Surface3d ::
    { s3x :: Scalar UvPoint
    , s3y :: Scalar UvPoint
    , s3z :: Scalar UvPoint
    , s3v :: ~Surface3dValueFunction
    , s3b :: ~Surface3dBoundsFunction
    } ->
    Expression UvPoint (Point3d (space @ units))
  VectorCurve3d ::
    { vc3x :: Scalar Float
    , vc3y :: Scalar Float
    , vc3z :: Scalar Float
    , vc3v :: ~Curve3dValueFunction
    , vc3b :: ~Curve3dBoundsFunction
    } ->
    Expression Float (Vector3d (space @ units))
  VectorSurface3d ::
    { vs3x :: Scalar UvPoint
    , vs3y :: Scalar UvPoint
    , vs3z :: Scalar UvPoint
    , vs3v :: ~Surface3dValueFunction
    , vs3b :: ~Surface3dBoundsFunction
    } ->
    Expression UvPoint (Vector3d (space @ units))

-- TODO special-case compiling of very simple expressions (constants or parameter values)
-- to pure Haskell functions, to avoid FFI overhead when the expression itself is trivial
--   -> maybe even have heuristics on 'size' of expression,
--      and/or whether it includes any repeated subexpressions

curve1d :: Scalar Float -> Expression Float (Qty units)
curve1d expression = do
  let (c1v, c1b) = case expression of
        Scalar.Constant x -> do
          let d = Float.toDouble x
          let fv = always d
          -- TODO: refactor this to avoid allocating and writing to temporary memory
          -- in the constant-curve special case
          let fb _ _ out = Foreign.pokeElemOff out 0 d >> Foreign.pokeElemOff out 1 d
          (fv, fb)
        _ -> do
          let px = Scalar.ptr expression
          let fv = curve1d_value_function (opensolid_curve1d_value_function px)
          let fb = curve1d_bounds_function (opensolid_curve1d_bounds_function px)
          (fv, fb)
  Curve1d{c1x = expression, c1v, c1b}

surface1d :: Scalar UvPoint -> Expression UvPoint (Qty units)
surface1d expression = do
  let px = Scalar.ptr expression
  Surface1d
    { s1x = expression
    , s1v = surface1d_value_function (opensolid_surface1d_value_function px)
    , s1b = surface1d_bounds_function (opensolid_surface1d_bounds_function px)
    }

curve2d :: Scalar Float -> Scalar Float -> Expression Float (Point2d (space @ units))
curve2d x y = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  Curve2d
    { c2x = x
    , c2y = y
    , c2v = curve2d_value_function (opensolid_curve2d_value_function px py)
    , c2b = curve2d_bounds_function (opensolid_curve2d_bounds_function px py)
    }

surface2d ::
  Scalar UvPoint ->
  Scalar UvPoint ->
  Expression UvPoint (Point2d (space @ units))
surface2d x y = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  Surface2d
    { s2x = x
    , s2y = y
    , s2v = surface2d_value_function (opensolid_surface2d_value_function px py)
    , s2b = surface2d_bounds_function (opensolid_surface2d_bounds_function px py)
    }

vectorCurve2d :: Scalar Float -> Scalar Float -> Expression Float (Vector2d (space @ units))
vectorCurve2d x y = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  VectorCurve2d
    { vc2x = x
    , vc2y = y
    , vc2v = curve2d_value_function (opensolid_curve2d_value_function px py)
    , vc2b = curve2d_bounds_function (opensolid_curve2d_bounds_function px py)
    }

vectorSurface2d ::
  Scalar UvPoint ->
  Scalar UvPoint ->
  Expression UvPoint (Vector2d (space @ units))
vectorSurface2d x y = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  VectorSurface2d
    { vs2x = x
    , vs2y = y
    , vs2v = surface2d_value_function (opensolid_surface2d_value_function px py)
    , vs2b = surface2d_bounds_function (opensolid_surface2d_bounds_function px py)
    }

curve3d ::
  Scalar Float ->
  Scalar Float ->
  Scalar Float ->
  Expression Float (Point3d (space @ units))
curve3d x y z = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  let pz = Scalar.ptr z
  Curve3d
    { c3x = x
    , c3y = y
    , c3z = z
    , c3v = curve3d_value_function (opensolid_curve3d_value_function px py pz)
    , c3b = curve3d_bounds_function (opensolid_curve3d_bounds_function px py pz)
    }

surface3d ::
  Scalar UvPoint ->
  Scalar UvPoint ->
  Scalar UvPoint ->
  Expression UvPoint (Point3d (space @ units))
surface3d x y z = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  let pz = Scalar.ptr z
  Surface3d
    { s3x = x
    , s3y = y
    , s3z = z
    , s3v = surface3d_value_function (opensolid_surface3d_value_function px py pz)
    , s3b = surface3d_bounds_function (opensolid_surface3d_bounds_function px py pz)
    }

vectorCurve3d ::
  Scalar Float ->
  Scalar Float ->
  Scalar Float ->
  Expression Float (Vector3d (space @ units))
vectorCurve3d x y z = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  let pz = Scalar.ptr z
  VectorCurve3d
    { vc3x = x
    , vc3y = y
    , vc3z = z
    , vc3v = curve3d_value_function (opensolid_curve3d_value_function px py pz)
    , vc3b = curve3d_bounds_function (opensolid_curve3d_bounds_function px py pz)
    }

vectorSurface3d ::
  Scalar UvPoint ->
  Scalar UvPoint ->
  Scalar UvPoint ->
  Expression UvPoint (Vector3d (space @ units))
vectorSurface3d x y z = do
  let px = Scalar.ptr x
  let py = Scalar.ptr y
  let pz = Scalar.ptr z
  VectorSurface3d
    { vs3x = x
    , vs3y = y
    , vs3z = z
    , vs3v = surface3d_value_function (opensolid_surface3d_value_function px py pz)
    , vs3b = surface3d_bounds_function (opensolid_surface3d_bounds_function px py pz)
    }

-------------
--- UNITS ---
-------------

instance
  HasUnits
    (Expression input (Qty units))
    units
    (Expression input (Qty Unitless))

instance
  HasUnits
    (Expression input (Vector2d (space @ units)))
    units
    (Expression input (Vector2d (space @ Unitless)))

instance
  HasUnits
    (Expression input (Vector3d (space @ units)))
    units
    (Expression input (Vector3d (space @ Unitless)))

instance
  HasUnits
    (Expression input (Point2d (space @ units)))
    units
    (Expression input (Point2d (space @ Unitless)))

instance
  HasUnits
    (Expression input (Point3d (space @ units)))
    units
    (Expression input (Point3d (space @ Unitless)))

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Qty units1))
    (Expression input2 (Qty units2))
  where
  coerce Curve1d{c1x, c1v, c1b} = Curve1d{c1x, c1v, c1b}
  coerce Surface1d{s1x, s1v, s1b} = Surface1d{s1x, s1v, s1b}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point2d (space @ units1)))
    (Expression input2 (Point2d (space @ units2)))
  where
  coerce Curve2d{c2x, c2y, c2v, c2b} = Curve2d{c2x, c2y, c2v, c2b}
  coerce Surface2d{s2x, s2y, s2v, s2b} = Surface2d{s2x, s2y, s2v, s2b}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Vector2d (space @ units2)))
  where
  coerce VectorCurve2d{vc2x, vc2y, vc2v, vc2b} = VectorCurve2d{vc2x, vc2y, vc2v, vc2b}
  coerce VectorSurface2d{vs2x, vs2y, vs2v, vs2b} = VectorSurface2d{vs2x, vs2y, vs2v, vs2b}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point3d (space @ units1)))
    (Expression input2 (Point3d (space @ units2)))
  where
  coerce Curve3d{c3x, c3y, c3z, c3v, c3b} = Curve3d{c3x, c3y, c3z, c3v, c3b}
  coerce Surface3d{s3x, s3y, s3z, s3v, s3b} = Surface3d{s3x, s3y, s3z, s3v, s3b}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector3d (space @ units1)))
    (Expression input2 (Vector3d (space @ units2)))
  where
  coerce VectorCurve3d{vc3x, vc3y, vc3z, vc3v, vc3b} =
    VectorCurve3d{vc3x, vc3y, vc3z, vc3v, vc3b}
  coerce VectorSurface3d{vs3x, vs3y, vs3z, vs3v, vs3b} =
    VectorSurface3d{vs3x, vs3y, vs3z, vs3v, vs3b}

----------------
--- NEGATION ---
----------------

instance Negation (Expression Float (Qty units)) where
  negate Curve1d{c1x} = curve1d -c1x

instance Negation (Expression UvPoint (Qty units)) where
  negate Surface1d{s1x} = surface1d -s1x

instance Negation (Expression Float (Vector2d (space @ units))) where
  negate VectorCurve2d{vc2x, vc2y} = vectorCurve2d -vc2x -vc2y

instance Negation (Expression UvPoint (Vector2d (space @ units))) where
  negate VectorSurface2d{vs2x, vs2y} = vectorSurface2d -vs2x -vs2y

instance Negation (Expression Float (Vector3d (space @ units))) where
  negate VectorCurve3d{vc3x, vc3y, vc3z} = vectorCurve3d -vc3x -vc3y -vc3z

instance Negation (Expression UvPoint (Vector3d (space @ units))) where
  negate VectorSurface3d{vs3x, vs3y, vs3z} = vectorSurface3d -vs3x -vs3y -vs3z

instance
  Multiplication
    Sign
    (Expression Float (Qty units))
    (Expression Float (Qty units))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Qty units))
    (Expression UvPoint (Qty units))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    (Expression Float (Qty units))
    Sign
    (Expression Float (Qty units))
  where
  expression * Positive = expression
  expression * Negative = -expression

instance
  Multiplication
    (Expression UvPoint (Qty units))
    Sign
    (Expression UvPoint (Qty units))
  where
  expression * Positive = expression
  expression * Negative = -expression

instance
  Multiplication
    Sign
    (Expression Float (Vector2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Vector2d (space @ units)))
    (Expression UvPoint (Vector2d (space @ units)))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    (Expression Float (Vector2d (space @ units)))
    Sign
    (Expression Float (Vector2d (space @ units)))
  where
  expression * Positive = expression
  expression * Negative = -expression

instance
  Multiplication
    (Expression UvPoint (Vector2d (space @ units)))
    Sign
    (Expression UvPoint (Vector2d (space @ units)))
  where
  expression * Positive = expression
  expression * Negative = -expression

instance
  Multiplication
    Sign
    (Expression Float (Vector3d (space @ units)))
    (Expression Float (Vector3d (space @ units)))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Vector3d (space @ units)))
    (Expression UvPoint (Vector3d (space @ units)))
  where
  Positive * expression = expression
  Negative * expression = -expression

instance
  Multiplication
    (Expression Float (Vector3d (space @ units)))
    Sign
    (Expression Float (Vector3d (space @ units)))
  where
  expression * Positive = expression
  expression * Negative = -expression

instance
  Multiplication
    (Expression UvPoint (Vector3d (space @ units)))
    Sign
    (Expression UvPoint (Vector3d (space @ units)))
  where
  expression * Positive = expression
  expression * Negative = -expression

----------------
--- ADDITION ---
----------------

instance
  units1 ~ units2 =>
  Addition
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units1))
  where
  Curve1d{c1x = lhs} + Curve1d{c1x = rhs} = curve1d (lhs + rhs)

instance
  units1 ~ units2 =>
  Addition
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units1))
  where
  Surface1d{s1x = lhs} + Surface1d{s1x = rhs} = surface1d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} + VectorCurve2d{vc2x = x2, vc2y = y2} =
    vectorCurve2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} + VectorSurface2d{vs2x = x2, vs2y = y2} =
    vectorSurface2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1}
    + VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
      vectorCurve3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    + VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      vectorSurface3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} + VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Point2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} + VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} + VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    curve3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Point3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} + VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
    surface3d (x1 + x2) (y1 + y2) (z1 + z2)

-------------------
--- SUBTRACTION ---
-------------------

instance
  units1 ~ units2 =>
  Subtraction
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units1))
  where
  Curve1d{c1x = lhs} - Curve1d{c1x = rhs} = curve1d (lhs - rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units1))
  where
  Surface1d{s1x = lhs} - Surface1d{s1x = rhs} = surface1d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} - VectorCurve2d{vc2x = x2, vc2y = y2} =
    vectorCurve2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} - VectorSurface2d{vs2x = x2, vs2y = y2} =
    vectorSurface2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1} - VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    vectorCurve3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    - VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      vectorSurface3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} - VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Point2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} - VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Point2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} - Curve2d{c2x = x2, c2y = y2} =
    vectorCurve2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Point2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} - Surface2d{s2x = x2, s2y = y2} =
    vectorSurface2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} - VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    curve3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Point3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} - VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
    surface3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Point3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} - Curve3d{c3x = x2, c3y = y2, c3z = z2} =
    vectorCurve3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Point3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} - Surface3d{s3x = x2, s3y = y2, s3z = z2} =
    vectorSurface3d (x1 - x2) (y1 - y2) (z1 - z2)

----------------------
--- MULTIPLICATION ---
----------------------

--------------------------------
--- Multiplication instances ---
--------------------------------

--- Qty-Qty ---
---------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

--- Qty-Vector2d ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Expression Float (Vector2d (space @ units2)))
    (Expression Float (Vector2d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector2d (space @ units2)))
    (Expression UvPoint (Vector2d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

--- Vector2d-Qty ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

--- Qty-Vector3d ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Expression Float (Vector3d (space @ units2)))
    (Expression Float (Vector3d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector3d (space @ units2)))
    (Expression UvPoint (Vector3d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

--- Vector3d-Qty ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ units3)))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

---------------------------------
--- Multiplication' instances ---
---------------------------------

--- Qty-Qty ---
---------------

instance
  Multiplication'
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty (units1 :*: units2)))
  where
  Curve1d{c1x = lhs} .*. Curve1d{c1x = rhs} = curve1d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  Surface1d{s1x = lhs} .*. Surface1d{s1x = rhs} = surface1d (lhs * rhs)

--- Qty-Vector2d ---
--------------------

instance
  Multiplication'
    (Expression Float (Qty units1))
    (Expression Float (Vector2d (space @ units2)))
    (Expression Float (Vector2d (space @ (units1 :*: units2))))
  where
  Curve1d{c1x = scale} .*. VectorCurve2d{vc2x, vc2y} =
    vectorCurve2d (scale * vc2x) (scale * vc2y)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector2d (space @ units2)))
    (Expression UvPoint (Vector2d (space @ (units1 :*: units2))))
  where
  Surface1d{s1x = scale} .*. VectorSurface2d{vs2x, vs2y} =
    vectorSurface2d (scale * vs2x) (scale * vs2y)

--- Vector2d-Qty ---
--------------------

instance
  Multiplication'
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ (units1 :*: units2))))
  where
  VectorCurve2d{vc2x, vc2y} .*. Curve1d{c1x = scale} =
    vectorCurve2d (vc2x * scale) (vc2y * scale)

instance
  Multiplication'
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ (units1 :*: units2))))
  where
  VectorSurface2d{vs2x, vs2y} .*. Surface1d{s1x = scale} =
    vectorSurface2d (vs2x * scale) (vs2y * scale)

--- Qty-Vector3d ---
--------------------

instance
  Multiplication'
    (Expression Float (Qty units1))
    (Expression Float (Vector3d (space @ units2)))
    (Expression Float (Vector3d (space @ (units1 :*: units2))))
  where
  Curve1d{c1x = scale} .*. VectorCurve3d{vc3x, vc3y, vc3z} =
    vectorCurve3d (scale * vc3x) (scale * vc3y) (scale * vc3z)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector3d (space @ units2)))
    (Expression UvPoint (Vector3d (space @ (units1 :*: units2))))
  where
  Surface1d{s1x = scale} .*. VectorSurface3d{vs3x, vs3y, vs3z} =
    vectorSurface3d (scale * vs3x) (scale * vs3y) (scale * vs3z)

--- Vector3d-Qty ---
--------------------

instance
  Multiplication'
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ (units1 :*: units2))))
  where
  VectorCurve3d{vc3x, vc3y, vc3z} .*. Curve1d{c1x = scale} =
    vectorCurve3d (vc3x * scale) (vc3y * scale) (vc3z * scale)

instance
  Multiplication'
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ (units1 :*: units2))))
  where
  VectorSurface3d{vs3x, vs3y, vs3z} .*. Surface1d{s1x = scale} =
    vectorSurface3d (vs3x * scale) (vs3y * scale) (vs3z * scale)

----------------
--- DIVISION ---
----------------

--- Division instances ---
--------------------------

--- Qty-Qty ---
---------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

--- Vector2d-Qty ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ units3)))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

--- Vector3d-Qty ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ units3)))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ units3)))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

---------------------------
--- Division' instances ---
---------------------------

--- Qty-Qty ---
---------------

instance
  Division'
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty (units1 :/: units2)))
  where
  Curve1d{c1x = lhs} ./. Curve1d{c1x = rhs} = curve1d (lhs / rhs)

instance
  Division'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty (units1 :/: units2)))
  where
  Surface1d{s1x = lhs} ./. Surface1d{s1x = rhs} = surface1d (lhs / rhs)

--- Vector2d-Qty ---
--------------------

instance
  Division'
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ (units1 :/: units2))))
  where
  VectorCurve2d{vc2x, vc2y} ./. Curve1d{c1x = scale} =
    vectorCurve2d (vc2x / scale) (vc2y / scale)

instance
  Division'
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ (units1 :/: units2))))
  where
  VectorSurface2d{vs2x, vs2y} ./. Surface1d{s1x = scale} =
    vectorSurface2d (vs2x / scale) (vs2y / scale)

--- Vector3d-Qty ---
--------------------

instance
  Division'
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ (units1 :/: units2))))
  where
  VectorCurve3d{vc3x, vc3y, vc3z} ./. Curve1d{c1x = scale} =
    vectorCurve3d (vc3x / scale) (vc3y / scale) (vc3z / scale)

instance
  Division'
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ (units1 :/: units2))))
  where
  VectorSurface3d{vs3x, vs3y, vs3z} ./. Surface1d{s1x = scale} =
    vectorSurface3d (vs3x / scale) (vs3y / scale) (vs3z / scale)

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Qty units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Qty units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

--- DotMultiplication' instances ---
------------------------------------

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty (units1 :*: units2)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} `dot'` VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve1d (x1 * x2 + y1 * y2)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} `dot'` VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface1d (x1 * x2 + y1 * y2)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Qty (units1 :*: units2)))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1}
    `dot'` VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
      curve1d (x1 * x2 + y1 * y2 + z1 * z2)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    `dot'` VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      surface1d (x1 * x2 + y1 * y2 + z1 * z2)

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units3)))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units3)))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

--- CrossMultiplication' instances ---
--------------------------------------

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty (units1 :*: units2)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} `cross'` VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve1d (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} `cross'` VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface1d (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ (units1 :*: units2))))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1}
    `cross'` VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
      vectorCurve3d
        (y1 * z2 - z1 * y2)
        (z1 * x2 - x1 * z2)
        (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ (units1 :*: units2))))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    `cross'` VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      vectorSurface3d
        (y1 * z2 - z1 * y2)
        (z1 * x2 - x1 * z2)
        (x1 * y2 - y1 * x2)

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Expression Float Float)
    (Expression Float output)
    (Expression Float output)
  where
  curve . Curve1d{c1x = input} =
    case curve of
      Curve1d{c1x} -> curve1d (c1x . input)
      Curve2d{c2x, c2y} -> curve2d (c2x . input) (c2y . input)
      VectorCurve2d{vc2x, vc2y} -> vectorCurve2d (vc2x . input) (vc2y . input)
      Curve3d{c3x, c3y, c3z} -> curve3d (c3x . input) (c3y . input) (c3z . input)
      VectorCurve3d{vc3x, vc3y, vc3z} -> vectorCurve3d (vc3x . input) (vc3y . input) (vc3z . input)

instance
  Composition
    (Expression UvPoint Float)
    (Expression Float output)
    (Expression UvPoint output)
  where
  curve . Surface1d{s1x = input} =
    case curve of
      Curve1d{c1x} -> surface1d (c1x . input)
      Curve2d{c2x, c2y} -> surface2d (c2x . input) (c2y . input)
      VectorCurve2d{vc2x, vc2y} -> vectorSurface2d (vc2x . input) (vc2y . input)
      Curve3d{c3x, c3y, c3z} -> surface3d (c3x . input) (c3y . input) (c3z . input)
      VectorCurve3d{vc3x, vc3y, vc3z} ->
        vectorSurface3d (vc3x . input) (vc3y . input) (vc3z . input)

instance
  Composition
    (Expression Float UvPoint)
    (Expression UvPoint output)
    (Expression Float output)
  where
  surface . Curve2d{c2x = uInput, c2y = vInput} = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d{s1x} -> curve1d (s1x . inputs)
      Surface2d{s2x, s2y} -> curve2d (s2x . inputs) (s2y . inputs)
      VectorSurface2d{vs2x, vs2y} -> vectorCurve2d (vs2x . inputs) (vs2y . inputs)
      Surface3d{s3x, s3y, s3z} -> curve3d (s3x . inputs) (s3y . inputs) (s3z . inputs)
      VectorSurface3d{vs3x, vs3y, vs3z} ->
        vectorCurve3d (vs3x . inputs) (vs3y . inputs) (vs3z . inputs)

instance
  Composition
    (Expression UvPoint UvPoint)
    (Expression UvPoint output)
    (Expression UvPoint output)
  where
  surface . Surface2d{s2x = uInput, s2y = vInput} = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d{s1x} -> surface1d (s1x . inputs)
      Surface2d{s2x, s2y} -> surface2d (s2x . inputs) (s2y . inputs)
      VectorSurface2d{vs2x, vs2y} -> vectorSurface2d (vs2x . inputs) (vs2y . inputs)
      Surface3d{s3x, s3y, s3z} -> surface3d (s3x . inputs) (s3y . inputs) (s3z . inputs)
      VectorSurface3d{vs3x, vs3y, vs3z} ->
        vectorSurface3d (vs3x . inputs) (vs3y . inputs) (vs3z . inputs)

-----------------
--- FUNCTIONS ---
-----------------

class Zero input output where
  zero :: Expression input output

instance Zero Float (Qty units) where
  zero = constant Qty.zero

instance Zero UvPoint (Qty units) where
  zero = constant Qty.zero

instance Zero Float (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero UvPoint (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero Float (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

instance Zero UvPoint (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

class Origin input output where
  origin :: Expression input output

instance Origin Float (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin UvPoint (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Float (Point3d (space @ units)) where
  origin = constant Point3d.origin

instance Origin UvPoint (Point3d (space @ units)) where
  origin = constant Point3d.origin

class Constant input output where
  constant :: output -> Expression input output

instance Constant Float (Qty units) where
  constant qty = curve1d (Scalar.constant qty)

instance Constant UvPoint (Qty units) where
  constant qty = surface1d (Scalar.constant qty)

instance Constant Float (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    vectorCurve2d (Scalar.constant x) (Scalar.constant y)

instance Constant UvPoint (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    vectorSurface2d (Scalar.constant x) (Scalar.constant y)

instance Constant Float (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    vectorCurve3d (Scalar.constant x) (Scalar.constant y) (Scalar.constant z)

instance Constant UvPoint (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    vectorSurface3d (Scalar.constant x) (Scalar.constant y) (Scalar.constant z)

instance Constant Float (Point2d (space @ units)) where
  constant (Point2d x y) =
    curve2d (Scalar.constant x) (Scalar.constant y)

instance Constant UvPoint (Point2d (space @ units)) where
  constant (Point2d x y) =
    surface2d (Scalar.constant x) (Scalar.constant y)

instance Constant Float (Point3d (space @ units)) where
  constant (Point3d x y z) =
    curve3d (Scalar.constant x) (Scalar.constant y) (Scalar.constant z)

instance Constant UvPoint (Point3d (space @ units)) where
  constant (Point3d x y z) =
    surface3d (Scalar.constant x) (Scalar.constant y) (Scalar.constant z)

class XY input output units | output -> units where
  xy ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input output

instance XY Float (Vector2d (space @ units)) units where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = vectorCurve2d x y

instance XY UvPoint (Vector2d (space @ units)) units where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = vectorSurface2d x y

instance XY Float (Point2d (space @ units)) units where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = curve2d x y

instance XY UvPoint (Point2d (space @ units)) units where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = surface2d x y

class XYZ input output units | output -> units where
  xyz ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input output

instance XYZ Float (Vector3d (space @ units)) units where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = vectorCurve3d x y z

instance XYZ UvPoint (Vector3d (space @ units)) units where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = vectorSurface3d x y z

instance XYZ Float (Point3d (space @ units)) units where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = curve3d x y z

instance XYZ UvPoint (Point3d (space @ units)) units where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = surface3d x y z

class XComponent input output units | output -> units where
  xComponent :: Expression input output -> Expression input (Qty units)

class YComponent input output units | output -> units where
  yComponent :: Expression input output -> Expression input (Qty units)

class ZComponent input output units | output -> units where
  zComponent :: Expression input output -> Expression input (Qty units)

instance XComponent input (Vector2d (space @ units)) units where
  xComponent VectorCurve2d{vc2x} = curve1d vc2x
  xComponent VectorSurface2d{vs2x} = surface1d vs2x

instance XComponent input (Vector3d (space @ units)) units where
  xComponent VectorCurve3d{vc3x} = curve1d vc3x
  xComponent VectorSurface3d{vs3x} = surface1d vs3x

instance YComponent input (Vector2d (space @ units)) units where
  yComponent VectorCurve2d{vc2y} = curve1d vc2y
  yComponent VectorSurface2d{vs2y} = surface1d vs2y

instance YComponent input (Vector3d (space @ units)) units where
  yComponent VectorCurve3d{vc3y} = curve1d vc3y
  yComponent VectorSurface3d{vs3y} = surface1d vs3y

instance ZComponent input (Vector3d (space @ units)) units where
  zComponent VectorCurve3d{vc3z} = curve1d vc3z
  zComponent VectorSurface3d{vs3z} = surface1d vs3z

class XCoordinate input output units | output -> units where
  xCoordinate :: Expression input output -> Expression input (Qty units)

class YCoordinate input output units | output -> units where
  yCoordinate :: Expression input output -> Expression input (Qty units)

class ZCoordinate input output units | output -> units where
  zCoordinate :: Expression input output -> Expression input (Qty units)

instance XCoordinate input (Point2d (space @ units)) units where
  xCoordinate Curve2d{c2x} = curve1d c2x
  xCoordinate Surface2d{s2x} = surface1d s2x

instance XCoordinate input (Point3d (space @ units)) units where
  xCoordinate Curve3d{c3x} = curve1d c3x
  xCoordinate Surface3d{s3x} = surface1d s3x

instance YCoordinate input (Point2d (space @ units)) units where
  yCoordinate Curve2d{c2y} = curve1d c2y
  yCoordinate Surface2d{s2y} = surface1d s2y

instance YCoordinate input (Point3d (space @ units)) units where
  yCoordinate Curve3d{c3y} = curve1d c3y
  yCoordinate Surface3d{s3y} = surface1d s3y

instance ZCoordinate input (Point3d (space @ units)) units where
  zCoordinate Curve3d{c3z} = curve1d c3z
  zCoordinate Surface3d{s3z} = surface1d s3z

t :: Expression Float Float
t = curve1d Scalar.curveParameter

r :: Expression Float Float
r = constant @Float 1.0 - t

u :: Expression UvPoint Float
u = surface1d (Scalar.surfaceParameter SurfaceParameter.U)

v :: Expression UvPoint Float
v = surface1d (Scalar.surfaceParameter SurfaceParameter.V)

squared' :: Expression input (Qty units) -> Expression input (Qty (units :*: units))
squared' Curve1d{c1x} = curve1d (Scalar.squared c1x)
squared' Surface1d{s1x} = surface1d (Scalar.squared s1x)

squared ::
  Units.Squared units1 units2 =>
  Expression input (Qty units1) ->
  Expression input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Expression input (Qty (units :*: units)) -> Expression input (Qty units)
sqrt' Curve1d{c1x} = curve1d (Scalar.sqrt c1x)
sqrt' Surface1d{s1x} = surface1d (Scalar.sqrt s1x)

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Qty units2) ->
  Expression input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Expression input Angle -> Expression input Float
sin Curve1d{c1x} = curve1d (Scalar.sin c1x)
sin Surface1d{s1x} = surface1d (Scalar.sin s1x)

cos :: Expression input Angle -> Expression input Float
cos Curve1d{c1x} = curve1d (Scalar.cos c1x)
cos Surface1d{s1x} = surface1d (Scalar.cos s1x)

class QuadraticSpline output where
  quadraticSpline :: output -> output -> output -> Expression Float output

instance QuadraticSpline (Qty units) where
  quadraticSpline p1 p2 p3 = curve1d (Scalar.quadraticSpline p1 p2 p3 Scalar.curveParameter)

instance QuadraticSpline (Point2d (space @ units)) where
  quadraticSpline (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) =
    curve2d
      (Scalar.quadraticSpline x1 x2 x3 Scalar.curveParameter)
      (Scalar.quadraticSpline y1 y2 y3 Scalar.curveParameter)

instance QuadraticSpline (Vector2d (space @ units)) where
  quadraticSpline (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) =
    vectorCurve2d
      (Scalar.quadraticSpline x1 x2 x3 Scalar.curveParameter)
      (Scalar.quadraticSpline y1 y2 y3 Scalar.curveParameter)

instance QuadraticSpline (Point3d (space @ units)) where
  quadraticSpline (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) =
    curve3d
      (Scalar.quadraticSpline x1 x2 x3 Scalar.curveParameter)
      (Scalar.quadraticSpline y1 y2 y3 Scalar.curveParameter)
      (Scalar.quadraticSpline z1 z2 z3 Scalar.curveParameter)

instance QuadraticSpline (Vector3d (space @ units)) where
  quadraticSpline (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) =
    vectorCurve3d
      (Scalar.quadraticSpline x1 x2 x3 Scalar.curveParameter)
      (Scalar.quadraticSpline y1 y2 y3 Scalar.curveParameter)
      (Scalar.quadraticSpline z1 z2 z3 Scalar.curveParameter)

class CubicSpline output where
  cubicSpline :: output -> output -> output -> output -> Expression Float output

instance CubicSpline (Qty units) where
  cubicSpline p1 p2 p3 p4 = curve1d (Scalar.cubicSpline p1 p2 p3 p4 Scalar.curveParameter)

instance CubicSpline (Point2d (space @ units)) where
  cubicSpline (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) =
    curve2d
      (Scalar.cubicSpline x1 x2 x3 x4 Scalar.curveParameter)
      (Scalar.cubicSpline y1 y2 y3 y4 Scalar.curveParameter)

instance CubicSpline (Vector2d (space @ units)) where
  cubicSpline (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) =
    vectorCurve2d
      (Scalar.cubicSpline x1 x2 x3 x4 Scalar.curveParameter)
      (Scalar.cubicSpline y1 y2 y3 y4 Scalar.curveParameter)

instance CubicSpline (Point3d (space @ units)) where
  cubicSpline (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) =
    curve3d
      (Scalar.cubicSpline x1 x2 x3 x4 Scalar.curveParameter)
      (Scalar.cubicSpline y1 y2 y3 y4 Scalar.curveParameter)
      (Scalar.cubicSpline z1 z2 z3 z4 Scalar.curveParameter)

instance CubicSpline (Vector3d (space @ units)) where
  cubicSpline (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) =
    vectorCurve3d
      (Scalar.cubicSpline x1 x2 x3 x4 Scalar.curveParameter)
      (Scalar.cubicSpline y1 y2 y3 y4 Scalar.curveParameter)
      (Scalar.cubicSpline z1 z2 z3 z4 Scalar.curveParameter)

class BezierCurve output where
  bezierCurve :: NonEmpty output -> Expression Float output

instance BezierCurve (Qty units) where
  bezierCurve controlPoints = curve1d (Scalar.bezierCurve controlPoints Scalar.curveParameter)

instance BezierCurve (Point2d (space @ units)) where
  bezierCurve controlPoints =
    curve2d
      (Scalar.bezierCurve (NonEmpty.map Point2d.xCoordinate controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Point2d.yCoordinate controlPoints) Scalar.curveParameter)

instance BezierCurve (Vector2d (space @ units)) where
  bezierCurve controlPoints =
    vectorCurve2d
      (Scalar.bezierCurve (NonEmpty.map Vector2d.xComponent controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Vector2d.yComponent controlPoints) Scalar.curveParameter)

instance BezierCurve (Point3d (space @ units)) where
  bezierCurve controlPoints =
    curve3d
      (Scalar.bezierCurve (NonEmpty.map Point3d.xCoordinate controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Point3d.yCoordinate controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Point3d.zCoordinate controlPoints) Scalar.curveParameter)

instance BezierCurve (Vector3d (space @ units)) where
  bezierCurve controlPoints =
    vectorCurve3d
      (Scalar.bezierCurve (NonEmpty.map Vector3d.xComponent controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Vector3d.yComponent controlPoints) Scalar.curveParameter)
      (Scalar.bezierCurve (NonEmpty.map Vector3d.zComponent controlPoints) Scalar.curveParameter)

-----------------
--- COMPILING ---
-----------------

type Curve1dValueFunction = Double -> Double

type Curve1dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface1dValueFunction = Double -> Double -> Double

type Surface1dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

type Curve2dValueFunction = Double -> Ptr Double -> IO ()

type Curve2dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dValueFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

type Curve3dValueFunction = Double -> Ptr Double -> IO ()

type Curve3dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface3dValueFunction = Double -> Double -> Ptr Double -> IO ()

type Surface3dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "opensolid_curve1d_value_function"
  opensolid_curve1d_value_function :: Scalar.Ptr -> FunPtr Curve1dValueFunction

foreign import ccall unsafe "opensolid_curve1d_bounds_function"
  opensolid_curve1d_bounds_function :: Scalar.Ptr -> FunPtr Curve1dBoundsFunction

foreign import ccall unsafe "opensolid_surface1d_value_function"
  opensolid_surface1d_value_function :: Scalar.Ptr -> FunPtr Surface1dValueFunction

foreign import ccall unsafe "opensolid_surface1d_bounds_function"
  opensolid_surface1d_bounds_function :: Scalar.Ptr -> FunPtr Surface1dBoundsFunction

foreign import ccall unsafe "opensolid_curve2d_value_function"
  opensolid_curve2d_value_function :: Scalar.Ptr -> Scalar.Ptr -> FunPtr Curve2dValueFunction

foreign import ccall unsafe "opensolid_curve2d_bounds_function"
  opensolid_curve2d_bounds_function :: Scalar.Ptr -> Scalar.Ptr -> FunPtr Curve2dBoundsFunction

foreign import ccall unsafe "opensolid_surface2d_value_function"
  opensolid_surface2d_value_function :: Scalar.Ptr -> Scalar.Ptr -> FunPtr Surface2dValueFunction

foreign import ccall unsafe "opensolid_surface2d_bounds_function"
  opensolid_surface2d_bounds_function :: Scalar.Ptr -> Scalar.Ptr -> FunPtr Surface2dBoundsFunction

foreign import ccall unsafe "opensolid_curve3d_value_function"
  opensolid_curve3d_value_function :: Scalar.Ptr -> Scalar.Ptr -> Scalar.Ptr -> FunPtr Curve3dValueFunction

foreign import ccall unsafe "opensolid_curve3d_bounds_function"
  opensolid_curve3d_bounds_function :: Scalar.Ptr -> Scalar.Ptr -> Scalar.Ptr -> FunPtr Curve3dBoundsFunction

foreign import ccall unsafe "opensolid_surface3d_value_function"
  opensolid_surface3d_value_function :: Scalar.Ptr -> Scalar.Ptr -> Scalar.Ptr -> FunPtr Surface3dValueFunction

foreign import ccall unsafe "opensolid_surface3d_bounds_function"
  opensolid_surface3d_bounds_function :: Scalar.Ptr -> Scalar.Ptr -> Scalar.Ptr -> FunPtr Surface3dBoundsFunction

foreign import ccall unsafe "dynamic"
  curve1d_value_function :: FunPtr Curve1dValueFunction -> Curve1dValueFunction

foreign import ccall unsafe "dynamic"
  curve1d_bounds_function :: FunPtr Curve1dBoundsFunction -> Curve1dBoundsFunction

foreign import ccall unsafe "dynamic"
  surface1d_value_function :: FunPtr Surface1dValueFunction -> Surface1dValueFunction

foreign import ccall unsafe "dynamic"
  surface1d_bounds_function :: FunPtr Surface1dBoundsFunction -> Surface1dBoundsFunction

foreign import ccall unsafe "dynamic"
  curve2d_value_function :: FunPtr Curve2dValueFunction -> Curve2dValueFunction

foreign import ccall unsafe "dynamic"
  curve2d_bounds_function :: FunPtr Curve2dBoundsFunction -> Curve2dBoundsFunction

foreign import ccall unsafe "dynamic"
  surface2d_value_function :: FunPtr Surface2dValueFunction -> Surface2dValueFunction

foreign import ccall unsafe "dynamic"
  surface2d_bounds_function :: FunPtr Surface2dBoundsFunction -> Surface2dBoundsFunction

foreign import ccall unsafe "dynamic"
  curve3d_value_function :: FunPtr Curve3dValueFunction -> Curve3dValueFunction

foreign import ccall unsafe "dynamic"
  curve3d_bounds_function :: FunPtr Curve3dBoundsFunction -> Curve3dBoundsFunction

foreign import ccall unsafe "dynamic"
  surface3d_value_function :: FunPtr Surface3dValueFunction -> Surface3dValueFunction

foreign import ccall unsafe "dynamic"
  surface3d_bounds_function :: FunPtr Surface3dBoundsFunction -> Surface3dBoundsFunction

-- TODO perform garbage collection on JIT-compiled functions:
-- use GHC.Weak.mkWeak on f# to associate a finalizer with it
-- that calls a Rust function to delete the underlying JIT-compiled function/module

class
  Evaluation input output inputBounds outputBounds
    | input -> inputBounds
    , output -> outputBounds
  where
  evaluate :: Expression input output -> input -> output
  evaluateBounds :: Expression input output -> inputBounds -> outputBounds

instance
  Evaluation
    Float
    (Qty units)
    (Range Unitless)
    (Range units)
  where
  evaluate Curve1d{c1v} (Qty tValue) = Qty (c1v tValue)

  evaluateBounds Curve1d{c1b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 16
      c1b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty xLow) (Qty xHigh))

instance
  Evaluation
    UvPoint
    (Qty units)
    UvBounds
    (Range units)
  where
  evaluate Surface1d{s1v} uvPoint = do
    let Point2d (Qty uValue) (Qty vValue) = uvPoint
    Qty (s1v uValue vValue)

  evaluateBounds Surface1d{s1b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 16
      s1b uLow uHigh vLow vHigh outputs
      low <- Foreign.peekElemOff outputs 0
      high <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty low) (Qty high))

instance
  Evaluation
    Float
    (Point2d (space @ units))
    (Range Unitless)
    (Bounds2d (space @ units))
  where
  evaluate Curve2d{c2v} (Qty tValue) =
    unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      c2v tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

  evaluateBounds Curve2d{c2b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 32
      c2b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)

instance
  Evaluation
    UvPoint
    (Point2d (space @ units))
    UvBounds
    (Bounds2d (space @ units))
  where
  evaluate Surface2d{s2v} uvPoint =
    unsafeDupablePerformIO IO.do
      let Point2d (Qty uValue) (Qty vValue) = uvPoint
      outputs <- Alloc.mallocBytes 16
      s2v uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

  evaluateBounds Surface2d{s2b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 32
      s2b uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)

instance
  Evaluation
    Float
    (Vector2d (space @ units))
    (Range Unitless)
    (VectorBounds2d (space @ units))
  where
  evaluate VectorCurve2d{vc2v} (Qty tValue) =
    unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      vc2v tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Vector2d.xy (Qty px) (Qty py))

  evaluateBounds VectorCurve2d{vc2b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 32
      vc2b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (VectorBounds2d xRange yRange)

instance
  Evaluation
    UvPoint
    (Vector2d (space @ units))
    UvBounds
    (VectorBounds2d (space @ units))
  where
  evaluate VectorSurface2d{vs2v} uvPoint =
    unsafeDupablePerformIO IO.do
      let Point2d (Qty uValue) (Qty vValue) = uvPoint
      outputs <- Alloc.mallocBytes 16
      vs2v uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Vector2d.xy (Qty px) (Qty py))

  evaluateBounds VectorSurface2d{vs2b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 32
      vs2b uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (VectorBounds2d xRange yRange)

instance
  Evaluation
    Float
    (Point3d (space @ units))
    (Range Unitless)
    (Bounds3d (space @ units))
  where
  evaluate Curve3d{c3v} (Qty tValue) =
    unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 24
      c3v tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      pz <- Foreign.peekElemOff outputs 2
      Alloc.free outputs
      IO.succeed (Point3d.xyz (Qty px) (Qty py) (Qty pz))

  evaluateBounds Curve3d{c3b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 48
      c3b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      zLow <- Foreign.peekElemOff outputs 4
      zHigh <- Foreign.peekElemOff outputs 5
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      let zRange = Range (Qty zLow) (Qty zHigh)
      Alloc.free outputs
      IO.succeed (Bounds3d xRange yRange zRange)

instance
  Evaluation
    UvPoint
    (Point3d (space @ units))
    UvBounds
    (Bounds3d (space @ units))
  where
  evaluate Surface3d{s3v} uvPoint =
    unsafeDupablePerformIO IO.do
      let Point2d (Qty uValue) (Qty vValue) = uvPoint
      outputs <- Alloc.mallocBytes 24
      s3v uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      pz <- Foreign.peekElemOff outputs 2
      Alloc.free outputs
      IO.succeed (Point3d.xyz (Qty px) (Qty py) (Qty pz))

  evaluateBounds Surface3d{s3b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 48
      s3b uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      zLow <- Foreign.peekElemOff outputs 4
      zHigh <- Foreign.peekElemOff outputs 5
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      let zRange = Range (Qty zLow) (Qty zHigh)
      Alloc.free outputs
      IO.succeed (Bounds3d xRange yRange zRange)

instance
  Evaluation
    Float
    (Vector3d (space @ units))
    (Range Unitless)
    (VectorBounds3d (space @ units))
  where
  evaluate VectorCurve3d{vc3v} (Qty tValue) =
    unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 24
      vc3v tValue outputs
      vx <- Foreign.peekElemOff outputs 0
      vy <- Foreign.peekElemOff outputs 1
      vz <- Foreign.peekElemOff outputs 2
      Alloc.free outputs
      IO.succeed (Vector3d.xyz (Qty vx) (Qty vy) (Qty vz))

  evaluateBounds VectorCurve3d{vc3b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 48
      vc3b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      zLow <- Foreign.peekElemOff outputs 4
      zHigh <- Foreign.peekElemOff outputs 5
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      let zRange = Range (Qty zLow) (Qty zHigh)
      Alloc.free outputs
      IO.succeed (VectorBounds3d xRange yRange zRange)

instance
  Evaluation
    UvPoint
    (Vector3d (space @ units))
    UvBounds
    (VectorBounds3d (space @ units))
  where
  evaluate VectorSurface3d{vs3v} uvPoint =
    unsafeDupablePerformIO IO.do
      let Point2d (Qty uValue) (Qty vValue) = uvPoint
      outputs <- Alloc.mallocBytes 24
      vs3v uValue vValue outputs
      vx <- Foreign.peekElemOff outputs 0
      vy <- Foreign.peekElemOff outputs 1
      vz <- Foreign.peekElemOff outputs 2
      Alloc.free outputs
      IO.succeed (Vector3d.xyz (Qty vx) (Qty vy) (Qty vz))

  evaluateBounds VectorSurface3d{vs3b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 48
      vs3b uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      zLow <- Foreign.peekElemOff outputs 4
      zHigh <- Foreign.peekElemOff outputs 5
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      let zRange = Range (Qty zLow) (Qty zHigh)
      Alloc.free outputs
      IO.succeed (VectorBounds3d xRange yRange zRange)
