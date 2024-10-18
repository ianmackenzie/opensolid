module Function
  ( Function
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
  , parameter
  , u
  , v
  , sqrt
  , sqrt'
  , squared
  , squared'
  , sin
  , cos
  , CurveDerivative (curveDerivative)
  , SurfaceDerivative (surfaceDerivative)
  , Value (value)
  , Bounds (bounds)
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Direction2d (Direction2d)
import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import Function.Expression (Expression)
import Function.Expression qualified as Expression
import IO qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Point3d (Point3d (Point3d))
import Point3d qualified
import Qty (Qty (Qty))
import Qty qualified
import Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import Prelude (Double)

type role Function nominal nominal

data Function input output where
  Curve1d ::
    { c1x :: Expression Float
    , c1v :: ~Curve1dValueFunction
    , c1b :: ~Curve1dBoundsFunction
    , c1d :: ~(Function Float (Qty units))
    } ->
    Function Float (Qty units)
  Surface1d ::
    { s1x :: Expression Uv.Point
    , s1v :: ~Surface1dValueFunction
    , s1b :: ~Surface1dBoundsFunction
    , s1du :: ~(Function Uv.Point (Qty units))
    , s1dv :: ~(Function Uv.Point (Qty units))
    } ->
    Function Uv.Point (Qty units)
  Curve2d ::
    { c2x :: Expression Float
    , c2y :: Expression Float
    , c2v :: ~Curve2dValueFunction
    , c2b :: ~Curve2dBoundsFunction
    , c2d :: ~(Function Float (Vector2d (space @ units)))
    } ->
    Function Float (Point2d (space @ units))
  Surface2d ::
    { s2x :: Expression Uv.Point
    , s2y :: Expression Uv.Point
    , s2v :: ~Surface2dValueFunction
    , s2b :: ~Surface2dBoundsFunction
    , s2du :: ~(Function Uv.Point (Vector2d (space @ units)))
    , s2dv :: ~(Function Uv.Point (Vector2d (space @ units)))
    } ->
    Function Uv.Point (Point2d (space @ units))
  VectorCurve2d ::
    { vc2x :: Expression Float
    , vc2y :: Expression Float
    , vc2v :: ~Curve2dValueFunction
    , vc2b :: ~Curve2dBoundsFunction
    , vc2d :: ~(Function Float (Vector2d (space @ units)))
    } ->
    Function Float (Vector2d (space @ units))
  VectorSurface2d ::
    { vs2x :: Expression Uv.Point
    , vs2y :: Expression Uv.Point
    , vs2v :: ~Surface2dValueFunction
    , vs2b :: ~Surface2dBoundsFunction
    , vs2du :: ~(Function Uv.Point (Vector2d (space @ units)))
    , vs2dv :: ~(Function Uv.Point (Vector2d (space @ units)))
    } ->
    Function Uv.Point (Vector2d (space @ units))
  Curve3d ::
    { c3x :: Expression Float
    , c3y :: Expression Float
    , c3z :: Expression Float
    , c3v :: ~Curve3dValueFunction
    , c3b :: ~Curve3dBoundsFunction
    , c3d :: ~(Function Float (Vector3d (space @ units)))
    } ->
    Function Float (Point3d (space @ units))
  Surface3d ::
    { s3x :: Expression Uv.Point
    , s3y :: Expression Uv.Point
    , s3z :: Expression Uv.Point
    , s3v :: ~Surface3dValueFunction
    , s3b :: ~Surface3dBoundsFunction
    , s3du :: ~(Function Uv.Point (Vector3d (space @ units)))
    , s3dv :: ~(Function Uv.Point (Vector3d (space @ units)))
    } ->
    Function Uv.Point (Point3d (space @ units))
  VectorCurve3d ::
    { vc3x :: Expression Float
    , vc3y :: Expression Float
    , vc3z :: Expression Float
    , vc3v :: ~Curve3dValueFunction
    , vc3b :: ~Curve3dBoundsFunction
    , vc3d :: ~(Function Float (Vector3d (space @ units)))
    } ->
    Function Float (Vector3d (space @ units))
  VectorSurface3d ::
    { vs3x :: Expression Uv.Point
    , vs3y :: Expression Uv.Point
    , vs3z :: Expression Uv.Point
    , vs3v :: ~Surface3dValueFunction
    , vs3b :: ~Surface3dBoundsFunction
    , vs3du :: ~(Function Uv.Point (Vector3d (space @ units)))
    , vs3dv :: ~(Function Uv.Point (Vector3d (space @ units)))
    } ->
    Function Uv.Point (Vector3d (space @ units))

curve1d :: Expression Float -> Function Float (Qty units)
curve1d expression = do
  let px = Expression.ptr expression
  Curve1d
    { c1x = expression
    , c1v = curve1d_value_function (opensolid_curve1d_value_function px)
    , c1b = curve1d_bounds_function (opensolid_curve1d_bounds_function px)
    , c1d = curve1d (Expression.curveDerivative expression)
    }

surface1d :: Expression Uv.Point -> Function Uv.Point (Qty units)
surface1d expression = do
  let px = Expression.ptr expression
  let du = surface1d (Expression.surfaceDerivative U expression)
  let dv = surface1dv (Expression.surfaceDerivative V expression) du
  Surface1d
    { s1x = expression
    , s1v = surface1d_value_function (opensolid_surface1d_value_function px)
    , s1b = surface1d_bounds_function (opensolid_surface1d_bounds_function px)
    , s1du = du
    , s1dv = dv
    }

surface1dv :: Expression Uv.Point -> Function Uv.Point (Qty units) -> Function Uv.Point (Qty units)
surface1dv expression sibling = do
  let px = Expression.ptr expression
  let du = s1dv sibling
  let dv = surface1dv (Expression.surfaceDerivative V expression) du
  Surface1d
    { s1x = expression
    , s1v = surface1d_value_function (opensolid_surface1d_value_function px)
    , s1b = surface1d_bounds_function (opensolid_surface1d_bounds_function px)
    , s1du = du
    , s1dv = dv
    }

curve2d :: Expression Float -> Expression Float -> Function Float (Point2d (space @ units))
curve2d x y = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  Curve2d
    { c2x = x
    , c2y = y
    , c2v = curve2d_value_function (opensolid_curve2d_value_function px py)
    , c2b = curve2d_bounds_function (opensolid_curve2d_bounds_function px py)
    , c2d = vectorCurve2d (Expression.curveDerivative x) (Expression.curveDerivative y)
    }

surface2d ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Point2d (space @ units))
surface2d x y = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let du = vectorSurface2d (Expression.surfaceDerivative U x) (Expression.surfaceDerivative U y)
  let dv = vectorSurface2dv (Expression.surfaceDerivative V x) (Expression.surfaceDerivative V y) du
  Surface2d
    { s2x = x
    , s2y = y
    , s2v = surface2d_value_function (opensolid_surface2d_value_function px py)
    , s2b = surface2d_bounds_function (opensolid_surface2d_bounds_function px py)
    , s2du = du
    , s2dv = dv
    }

vectorCurve2d :: Expression Float -> Expression Float -> Function Float (Vector2d (space @ units))
vectorCurve2d x y = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  VectorCurve2d
    { vc2x = x
    , vc2y = y
    , vc2v = curve2d_value_function (opensolid_curve2d_value_function px py)
    , vc2b = curve2d_bounds_function (opensolid_curve2d_bounds_function px py)
    , vc2d = vectorCurve2d (Expression.curveDerivative x) (Expression.curveDerivative y)
    }

vectorSurface2d ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Vector2d (space @ units))
vectorSurface2d x y = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let du = vectorSurface2d (Expression.surfaceDerivative U x) (Expression.surfaceDerivative U y)
  let dv = vectorSurface2dv (Expression.surfaceDerivative V x) (Expression.surfaceDerivative V y) du
  VectorSurface2d
    { vs2x = x
    , vs2y = y
    , vs2v = surface2d_value_function (opensolid_surface2d_value_function px py)
    , vs2b = surface2d_bounds_function (opensolid_surface2d_bounds_function px py)
    , vs2du = du
    , vs2dv = dv
    }

vectorSurface2dv ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Vector2d (space @ units)) ->
  Function Uv.Point (Vector2d (space @ units))
vectorSurface2dv x y sibling = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let du = vs2dv sibling
  let dv = vectorSurface2dv (Expression.surfaceDerivative V x) (Expression.surfaceDerivative V y) du
  VectorSurface2d
    { vs2x = x
    , vs2y = y
    , vs2v = surface2d_value_function (opensolid_surface2d_value_function px py)
    , vs2b = surface2d_bounds_function (opensolid_surface2d_bounds_function px py)
    , vs2du = du
    , vs2dv = dv
    }

curve3d ::
  Expression Float ->
  Expression Float ->
  Expression Float ->
  Function Float (Point3d (space @ units))
curve3d x y z = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let pz = Expression.ptr z
  Curve3d
    { c3x = x
    , c3y = y
    , c3z = z
    , c3v = curve3d_value_function (opensolid_curve3d_value_function px py pz)
    , c3b = curve3d_bounds_function (opensolid_curve3d_bounds_function px py pz)
    , c3d =
        vectorCurve3d
          (Expression.curveDerivative x)
          (Expression.curveDerivative y)
          (Expression.curveDerivative z)
    }

surface3d ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Point3d (space @ units))
surface3d x y z = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let pz = Expression.ptr z
  let du =
        vectorSurface3d
          (Expression.surfaceDerivative U x)
          (Expression.surfaceDerivative U y)
          (Expression.surfaceDerivative U z)
  let dv =
        vectorSurface3dv
          (Expression.surfaceDerivative V x)
          (Expression.surfaceDerivative V y)
          (Expression.surfaceDerivative V z)
          du
  Surface3d
    { s3x = x
    , s3y = y
    , s3z = z
    , s3v = surface3d_value_function (opensolid_surface3d_value_function px py pz)
    , s3b = surface3d_bounds_function (opensolid_surface3d_bounds_function px py pz)
    , s3du = du
    , s3dv = dv
    }

vectorCurve3d ::
  Expression Float ->
  Expression Float ->
  Expression Float ->
  Function Float (Vector3d (space @ units))
vectorCurve3d x y z = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let pz = Expression.ptr z
  VectorCurve3d
    { vc3x = x
    , vc3y = y
    , vc3z = z
    , vc3v = curve3d_value_function (opensolid_curve3d_value_function px py pz)
    , vc3b = curve3d_bounds_function (opensolid_curve3d_bounds_function px py pz)
    , vc3d =
        vectorCurve3d
          (Expression.curveDerivative x)
          (Expression.curveDerivative y)
          (Expression.curveDerivative z)
    }

vectorSurface3d ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Vector3d (space @ units))
vectorSurface3d x y z = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let pz = Expression.ptr z
  let du =
        vectorSurface3d
          (Expression.surfaceDerivative U x)
          (Expression.surfaceDerivative U y)
          (Expression.surfaceDerivative U z)
  let dv =
        vectorSurface3dv
          (Expression.surfaceDerivative V x)
          (Expression.surfaceDerivative V y)
          (Expression.surfaceDerivative V z)
          du
  VectorSurface3d
    { vs3x = x
    , vs3y = y
    , vs3z = z
    , vs3v = surface3d_value_function (opensolid_surface3d_value_function px py pz)
    , vs3b = surface3d_bounds_function (opensolid_surface3d_bounds_function px py pz)
    , vs3du = du
    , vs3dv = dv
    }

vectorSurface3dv ::
  Expression Uv.Point ->
  Expression Uv.Point ->
  Expression Uv.Point ->
  Function Uv.Point (Vector3d (space @ units)) ->
  Function Uv.Point (Vector3d (space @ units))
vectorSurface3dv x y z sibling = do
  let px = Expression.ptr x
  let py = Expression.ptr y
  let pz = Expression.ptr z
  let du = vs3dv sibling
  let dv =
        vectorSurface3dv
          (Expression.surfaceDerivative V x)
          (Expression.surfaceDerivative V y)
          (Expression.surfaceDerivative V z)
          du
  VectorSurface3d
    { vs3x = x
    , vs3y = y
    , vs3z = z
    , vs3v = surface3d_value_function (opensolid_surface3d_value_function px py pz)
    , vs3b = surface3d_bounds_function (opensolid_surface3d_bounds_function px py pz)
    , vs3du = du
    , vs3dv = dv
    }

-------------
--- UNITS ---
-------------

instance HasUnits (Function input (Qty units)) where
  type UnitsOf (Function input (Qty units)) = units

instance HasUnits (Function input (Vector2d (space @ units))) where
  type UnitsOf (Function input (Vector2d (space @ units))) = units

instance HasUnits (Function input (Vector3d (space @ units))) where
  type UnitsOf (Function input (Vector3d (space @ units))) = units

instance HasUnits (Function input (Point2d (space @ units))) where
  type UnitsOf (Function input (Point2d (space @ units))) = units

instance HasUnits (Function input (Point3d (space @ units))) where
  type UnitsOf (Function input (Point3d (space @ units))) = units

instance
  input1 ~ input2 =>
  Units.Coercion
    (Function input1 (Qty units1))
    (Function input2 (Qty units2))
  where
  coerce Curve1d{c1x, c1v, c1b, c1d} = Curve1d{c1x, c1v, c1b, c1d = Units.coerce c1d}
  coerce Surface1d{s1x, s1v, s1b, s1du, s1dv} =
    Surface1d{s1x, s1v, s1b, s1du = Units.coerce s1du, s1dv = Units.coerce s1dv}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Point2d (space @ units1)))
    (Function input2 (Point2d (space @ units2)))
  where
  coerce Curve2d{c2x, c2y, c2v, c2b, c2d} = Curve2d{c2x, c2y, c2v, c2b, c2d = Units.coerce c2d}
  coerce Surface2d{s2x, s2y, s2v, s2b, s2du, s2dv} =
    Surface2d{s2x, s2y, s2v, s2b, s2du = Units.coerce s2du, s2dv = Units.coerce s2dv}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Vector2d (space @ units1)))
    (Function input2 (Vector2d (space @ units2)))
  where
  coerce VectorCurve2d{vc2x, vc2y, vc2v, vc2b, vc2d} =
    VectorCurve2d{vc2x, vc2y, vc2v, vc2b, vc2d = Units.coerce vc2d}
  coerce VectorSurface2d{vs2x, vs2y, vs2v, vs2b, vs2du, vs2dv} =
    VectorSurface2d{vs2x, vs2y, vs2v, vs2b, vs2du = Units.coerce vs2du, vs2dv = Units.coerce vs2dv}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Point3d (space @ units1)))
    (Function input2 (Point3d (space @ units2)))
  where
  coerce Curve3d{c3x, c3y, c3z, c3v, c3b, c3d} =
    Curve3d{c3x, c3y, c3z, c3v, c3b, c3d = Units.coerce c3d}
  coerce Surface3d{s3x, s3y, s3z, s3v, s3b, s3du, s3dv} =
    Surface3d{s3x, s3y, s3z, s3v, s3b, s3du = Units.coerce s3du, s3dv = Units.coerce s3dv}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Vector3d (space @ units1)))
    (Function input2 (Vector3d (space @ units2)))
  where
  coerce VectorCurve3d{vc3x, vc3y, vc3z, vc3v, vc3b, vc3d} =
    VectorCurve3d{vc3x, vc3y, vc3z, vc3v, vc3b, vc3d = Units.coerce vc3d}
  coerce VectorSurface3d{vs3x, vs3y, vs3z, vs3v, vs3b, vs3du, vs3dv} =
    VectorSurface3d
      { vs3x
      , vs3y
      , vs3z
      , vs3v
      , vs3b
      , vs3du = Units.coerce vs3du
      , vs3dv = Units.coerce vs3dv
      }

----------------
--- NEGATION ---
----------------

instance Negation (Function Float (Qty units)) where
  negate Curve1d{c1x} =
    curve1d (Expression.negated c1x)

instance Negation (Function Uv.Point (Qty units)) where
  negate Surface1d{s1x} =
    surface1d (Expression.negated s1x)

instance Negation (Function Float (Vector2d (space @ units))) where
  negate VectorCurve2d{vc2x, vc2y} =
    vectorCurve2d (Expression.negated vc2x) (Expression.negated vc2y)

instance Negation (Function Uv.Point (Vector2d (space @ units))) where
  negate VectorSurface2d{vs2x, vs2y} =
    vectorSurface2d (Expression.negated vs2x) (Expression.negated vs2y)

instance Negation (Function Float (Vector3d (space @ units))) where
  negate VectorCurve3d{vc3x, vc3y, vc3z} =
    vectorCurve3d (Expression.negated vc3x) (Expression.negated vc3y) (Expression.negated vc3z)

instance Negation (Function Uv.Point (Vector3d (space @ units))) where
  negate VectorSurface3d{vs3x, vs3y, vs3z} =
    vectorSurface3d (Expression.negated vs3x) (Expression.negated vs3y) (Expression.negated vs3z)

instance Multiplication' Sign (Function Float (Qty units)) where
  type Sign .*. Function Float (Qty units) = Function Float (Qty (Unitless :*: units))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' Sign (Function Uv.Point (Qty units)) where
  type Sign .*. Function Uv.Point (Qty units) = Function Uv.Point (Qty (Unitless :*: units))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function Float (Qty units)) Sign where
  type Function Float (Qty units) .*. Sign = Function Float (Qty (units :*: Unitless))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' (Function Uv.Point (Qty units)) Sign where
  type Function Uv.Point (Qty units) .*. Sign = Function Uv.Point (Qty (units :*: Unitless))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Function Float (Vector2d (space @ units))) where
  type
    Sign .*. Function Float (Vector2d (space @ units)) =
      Function Float (Vector2d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' Sign (Function Uv.Point (Vector2d (space @ units))) where
  type
    Sign .*. Function Uv.Point (Vector2d (space @ units)) =
      Function Uv.Point (Vector2d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function Float (Vector2d (space @ units))) Sign where
  type
    Function Float (Vector2d (space @ units)) .*. Sign =
      Function Float (Vector2d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' (Function Uv.Point (Vector2d (space @ units))) Sign where
  type
    Function Uv.Point (Vector2d (space @ units)) .*. Sign =
      Function Uv.Point (Vector2d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Function Float (Vector3d (space @ units))) where
  type
    Sign .*. Function Float (Vector3d (space @ units)) =
      Function Float (Vector3d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' Sign (Function Uv.Point (Vector3d (space @ units))) where
  type
    Sign .*. Function Uv.Point (Vector3d (space @ units)) =
      Function Uv.Point (Vector3d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function Float (Vector3d (space @ units))) Sign where
  type
    Function Float (Vector3d (space @ units)) .*. Sign =
      Function Float (Vector3d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' (Function Uv.Point (Vector3d (space @ units))) Sign where
  type
    Function Uv.Point (Vector3d (space @ units)) .*. Sign =
      Function Uv.Point (Vector3d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance
  Multiplication
    Sign
    (Function Float (Qty units))
    (Function Float (Qty units))

instance
  Multiplication
    Sign
    (Function Uv.Point (Qty units))
    (Function Uv.Point (Qty units))

instance
  Multiplication
    (Function Float (Qty units))
    Sign
    (Function Float (Qty units))

instance
  Multiplication
    (Function Uv.Point (Qty units))
    Sign
    (Function Uv.Point (Qty units))

instance
  Multiplication
    Sign
    (Function Float (Vector2d (space @ units)))
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    Sign
    (Function Uv.Point (Vector2d (space @ units)))
    (Function Uv.Point (Vector2d (space @ units)))

instance
  Multiplication
    (Function Float (Vector2d (space @ units)))
    Sign
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    (Function Uv.Point (Vector2d (space @ units)))
    Sign
    (Function Uv.Point (Vector2d (space @ units)))

instance
  Multiplication
    Sign
    (Function Float (Vector3d (space @ units)))
    (Function Float (Vector3d (space @ units)))

instance
  Multiplication
    Sign
    (Function Uv.Point (Vector3d (space @ units)))
    (Function Uv.Point (Vector3d (space @ units)))

instance
  Multiplication
    (Function Float (Vector3d (space @ units)))
    Sign
    (Function Float (Vector3d (space @ units)))

instance
  Multiplication
    (Function Uv.Point (Vector3d (space @ units)))
    Sign
    (Function Uv.Point (Vector3d (space @ units)))

----------------
--- ADDITION ---
----------------

instance
  units1 ~ units2 =>
  Addition
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units1))
  where
  Curve1d{c1x = lhs} + Curve1d{c1x = rhs} = curve1d (Expression.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units1))
  where
  Surface1d{s1x = lhs} + Surface1d{s1x = rhs} = surface1d (Expression.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units1))
  where
  expression + qty = expression + constant @Float qty

instance
  units1 ~ units2 =>
  Addition (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units1))
  where
  expression + qty = expression + constant @Uv.Point qty

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units1))
  where
  qty + expression = constant @Float qty + expression

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units1))
  where
  qty + expression = constant @Uv.Point qty + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} + VectorCurve2d{vc2x = x2, vc2y = y2} =
    vectorCurve2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} + VectorSurface2d{vs2x = x2, vs2y = y2} =
    vectorSurface2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  vector + expression = constant @Float vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  vector + expression = constant @Uv.Point vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1}
    + VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
      vectorCurve3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    + VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      vectorSurface3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  vector + expression = constant @Float vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  vector + expression = constant @Uv.Point vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} + VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} + VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Point2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  point + expression = constant @Float point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  point + expression = constant @Uv.Point point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} + VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    curve3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} + VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
    surface3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Point3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  point + expression = constant @Float point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  point + expression = constant @Uv.Point point + expression

-------------------
--- SUBTRACTION ---
-------------------

instance
  units1 ~ units2 =>
  Subtraction
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units1))
  where
  Curve1d{c1x = lhs} - Curve1d{c1x = rhs} = curve1d (Expression.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units1))
  where
  Surface1d{s1x = lhs} - Surface1d{s1x = rhs} = surface1d (Expression.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units1))
  where
  expression - qty = expression - constant @Float qty

instance
  units1 ~ units2 =>
  Subtraction (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units1))
  where
  expression - qty = expression - constant @Uv.Point qty

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units1))
  where
  qty - expression = constant @Float qty - expression

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units1))
  where
  qty - expression = constant @Uv.Point qty - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d{vc2x = x1, vc2y = y1} - VectorCurve2d{vc2x = x2, vc2y = y2} =
    vectorCurve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d{vs2x = x1, vs2y = y1} - VectorSurface2d{vs2x = x2, vs2y = y2} =
    vectorSurface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  vector - expression = constant @Float vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  vector - expression = constant @Uv.Point vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d{vc3x = x1, vc3y = y1, vc3z = z1} - VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    vectorCurve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d{vs3x = x1, vs3y = y1, vs3z = z1}
    - VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
      vectorSurface3d
        (Expression.difference x1 x2)
        (Expression.difference y1 y2)
        (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  vector - expression = constant @Float vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  vector - expression = constant @Uv.Point vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} - VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} - VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Point2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Point2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  Curve2d{c2x = x1, c2y = y1} - Curve2d{c2x = x2, c2y = y2} =
    vectorCurve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Point2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  Surface2d{s2x = x1, s2y = y1} - Surface2d{s2x = x2, s2y = y2} =
    vectorSurface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression - point = expression - constant @Float point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - point = expression - constant @Uv.Point point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Float (Point2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Uv.Point (Point2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} - VectorCurve3d{vc3x = x2, vc3y = y2, vc3z = z2} =
    curve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} - VectorSurface3d{vs3x = x2, vs3y = y2, vs3z = z2} =
    surface3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Point3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Point3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  Curve3d{c3x = x1, c3y = y1, c3z = z1} - Curve3d{c3x = x2, c3y = y2, c3z = z2} =
    vectorCurve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Point3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  Surface3d{s3x = x1, s3y = y1, s3z = z1} - Surface3d{s3x = x2, s3y = y2, s3z = z2} =
    vectorSurface3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression - point = expression - constant @Float point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - point = expression - constant @Uv.Point point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Float (Point3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Uv.Point (Point3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

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
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units3))

--- Qty-Vector2d ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Qty units1))
    (Function Float (Vector2d (space @ units2)))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Vector2d (space @ units2)))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Function Float (Vector2d (space @ units2)))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Function Uv.Point (Vector2d (space @ units2)))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Qty units1))
    (Vector2d (space @ units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Vector2d (space @ units2))
    (Function Uv.Point (Vector2d (space @ units3)))

-- Qty-Direction2d --
---------------------

instance
  Multiplication
    (Function Float (Qty units))
    (Direction2d space)
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    (Function Uv.Point (Qty units))
    (Direction2d space)
    (Function Uv.Point (Vector2d (space @ units)))

--- Vector2d-Qty ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Vector2d (space @ units1)))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Vector2d (space @ units1)))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Vector2d (space @ units1)))
    (Qty units2)
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Function Uv.Point (Vector2d (space @ units3)))

--- Direction2d-Qty ---
-----------------------

instance
  Multiplication
    (Direction2d space)
    (Function Float (Qty units))
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    (Direction2d space)
    (Function Uv.Point (Qty units))
    (Function Uv.Point (Vector2d (space @ units)))

---------------------------------
--- Multiplication' instances ---
---------------------------------

--- Qty-Qty ---
---------------

instance Multiplication' (Function Float (Qty units1)) (Function Float (Qty units2)) where
  type
    Function Float (Qty units1) .*. Function Float (Qty units2) =
      Function Float (Qty (units1 :*: units2))
  Curve1d{c1x = lhs} .*. Curve1d{c1x = rhs} = curve1d (Expression.product lhs rhs)

instance Multiplication' (Function Uv.Point (Qty units1)) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Qty units1) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  Surface1d{s1x = lhs} .*. Surface1d{s1x = rhs} = surface1d (Expression.product lhs rhs)

instance Multiplication' (Qty units1) (Function Float (Qty units2)) where
  type Qty units1 .*. Function Float (Qty units2) = Function Float (Qty (units1 :*: units2))
  qty .*. expression = constant @Float qty .*. expression

instance Multiplication' (Qty units1) (Function Uv.Point (Qty units2)) where
  type
    Qty units1 .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  qty .*. expression = constant @Uv.Point qty .*. expression

instance Multiplication' (Function Float (Qty units1)) (Qty units2) where
  type Function Float (Qty units1) .*. Qty units2 = Function Float (Qty (units1 :*: units2))
  expression .*. qty = expression .*. constant @Float qty

instance Multiplication' (Function Uv.Point (Qty units1)) (Qty units2) where
  type
    Function Uv.Point (Qty units1) .*. Qty units2 =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .*. qty = expression .*. constant @Uv.Point qty

--- Qty-Vector2d ---
--------------------

instance Multiplication' (Function Float (Qty units1)) (Function Float (Vector2d (space @ units2))) where
  type
    Function Float (Qty units1) .*. Function Float (Vector2d (space @ units2)) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  Curve1d{c1x = scale} .*. VectorCurve2d{vc2x, vc2y} =
    vectorCurve2d (Expression.product scale vc2x) (Expression.product scale vc2y)

instance Multiplication' (Function Uv.Point (Qty units1)) (Function Uv.Point (Vector2d (space @ units2))) where
  type
    Function Uv.Point (Qty units1) .*. Function Uv.Point (Vector2d (space @ units2)) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  Surface1d{s1x = scale} .*. VectorSurface2d{vs2x, vs2y} =
    vectorSurface2d (Expression.product scale vs2x) (Expression.product scale vs2y)

instance Multiplication' (Qty units1) (Function Float (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Function Float (Vector2d (space @ units2)) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  qty .*. expression = constant @Float qty .*. expression

instance Multiplication' (Qty units1) (Function Uv.Point (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Function Uv.Point (Vector2d (space @ units2)) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  qty .*. expression = constant @Uv.Point qty .*. expression

instance Multiplication' (Function Float (Qty units1)) (Vector2d (space @ units2)) where
  type
    Function Float (Qty units1) .*. Vector2d (space @ units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. constant @Float vector

instance Multiplication' (Function Uv.Point (Qty units1)) (Vector2d (space @ units2)) where
  type
    Function Uv.Point (Qty units1) .*. Vector2d (space @ units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. constant @Uv.Point vector

--- Qty-Direction2d ---
-----------------------

instance Multiplication' (Function Float (Qty units)) (Direction2d space) where
  type
    Function Float (Qty units) .*. Direction2d space =
      Function Float (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

instance Multiplication' (Function Uv.Point (Qty units)) (Direction2d space) where
  type
    Function Uv.Point (Qty units) .*. Direction2d space =
      Function Uv.Point (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

--- Vector2d-Qty ---
--------------------

instance Multiplication' (Function Float (Vector2d (space @ units1))) (Function Float (Qty units2)) where
  type
    Function Float (Vector2d (space @ units1)) .*. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  VectorCurve2d{vc2x, vc2y} .*. Curve1d{c1x = scale} =
    vectorCurve2d (Expression.product vc2x scale) (Expression.product vc2y scale)

instance Multiplication' (Function Uv.Point (Vector2d (space @ units1))) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Vector2d (space @ units1)) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  VectorSurface2d{vs2x, vs2y} .*. Surface1d{s1x = scale} =
    vectorSurface2d (Expression.product vs2x scale) (Expression.product vs2y scale)

instance Multiplication' (Vector2d (space @ units1)) (Function Float (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = constant @Float vector .*. expression

instance Multiplication' (Vector2d (space @ units1)) (Function Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = constant @Uv.Point vector .*. expression

instance Multiplication' (Function Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Float (Vector2d (space @ units1)) .*. Qty units2 =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. qty = expression .*. constant @Float qty

instance Multiplication' (Function Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Uv.Point (Vector2d (space @ units1)) .*. Qty units2 =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. qty = expression .*. constant @Uv.Point qty

--- Direction2d-Qty ---
-----------------------

instance Multiplication' (Direction2d space) (Function Float (Qty units)) where
  type
    Direction2d space .*. Function Float (Qty units) =
      Function Float (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

instance Multiplication' (Direction2d space) (Function Uv.Point (Qty units)) where
  type
    Direction2d space .*. Function Uv.Point (Qty units) =
      Function Uv.Point (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

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
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units3))

--- Vector2d-Qty ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Float (Vector2d (space @ units1)))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Vector2d (space @ units1)))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Float (Vector2d (space @ units1)))
    (Qty units2)
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Function Uv.Point (Vector2d (space @ units3)))

---------------------------
--- Division' instances ---
---------------------------

--- Qty-Qty ---
---------------

instance Division' (Function Float (Qty units1)) (Function Float (Qty units2)) where
  type
    Function Float (Qty units1) ./. Function Float (Qty units2) =
      Function Float (Qty (units1 :/: units2))
  Curve1d{c1x = lhs} ./. Curve1d{c1x = rhs} = curve1d (Expression.quotient lhs rhs)

instance Division' (Function Uv.Point (Qty units1)) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Qty units1) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :/: units2))
  Surface1d{s1x = lhs} ./. Surface1d{s1x = rhs} = surface1d (Expression.quotient lhs rhs)

instance Division' (Function Float (Qty units1)) (Qty units2) where
  type Function Float (Qty units1) ./. Qty units2 = Function Float (Qty (units1 :/: units2))
  expression ./. qty = expression ./. constant @Float qty

instance Division' (Function Uv.Point (Qty units1)) (Qty units2) where
  type
    Function Uv.Point (Qty units1) ./. Qty units2 =
      Function Uv.Point (Qty (units1 :/: units2))
  expression ./. qty = expression ./. constant @Uv.Point qty

instance Division' (Qty units1) (Function Float (Qty units2)) where
  type Qty units1 ./. Function Float (Qty units2) = Function Float (Qty (units1 :/: units2))
  qty ./. expression = constant @Float qty ./. expression

instance Division' (Qty units1) (Function Uv.Point (Qty units2)) where
  type
    Qty units1 ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :/: units2))
  qty ./. expression = constant @Uv.Point qty ./. expression

--- Vector2d-Qty ---
--------------------

instance Division' (Function Float (Vector2d (space @ units1))) (Function Float (Qty units2)) where
  type
    Function Float (Vector2d (space @ units1)) ./. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  VectorCurve2d{vc2x, vc2y} ./. Curve1d{c1x = scale} =
    vectorCurve2d (Expression.quotient vc2x scale) (Expression.quotient vc2y scale)

instance Division' (Function Uv.Point (Vector2d (space @ units1))) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Vector2d (space @ units1)) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  VectorSurface2d{vs2x, vs2y} ./. Surface1d{s1x = scale} =
    vectorSurface2d (Expression.quotient vs2x scale) (Expression.quotient vs2y scale)

instance Division' (Vector2d (space @ units1)) (Function Float (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = constant @Float vector ./. expression

instance Division' (Vector2d (space @ units1)) (Function Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = constant @Uv.Point vector ./. expression

instance Division' (Function Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Float (Vector2d (space @ units1)) ./. Qty units2 =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  expression ./. qty = expression ./. constant @Float qty

instance Division' (Function Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Uv.Point (Vector2d (space @ units1)) ./. Qty units2 =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  expression ./. qty = expression ./. constant @Uv.Point qty

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
    (Function Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Uv.Point (Qty units))

--- DotMultiplication' instances ---
------------------------------------

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Function Float (Vector2d (space1 @ units1))
      .<>. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  VectorCurve2d{vc2x = x1, vc2y = y1} .<>. VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve1d (Expression.sum (Expression.product x1 x2) (Expression.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1))
      .<>. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d{vs2x = x1, vs2y = y1} .<>. VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface1d (Expression.sum (Expression.product x1 x2) (Expression.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Float (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Function Float (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. constant @Float vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. constant @Uv.Point vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  vector .<>. expression = constant @Float vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  vector .<>. expression = constant @Uv.Point vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Float (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Function Float (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Uv.Point (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Function Uv.Point (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Function Float (Vector2d (space2 @ units)) =
      Function Float (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Function Uv.Point (Vector2d (space2 @ units)) =
      Function Uv.Point (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
    (Function Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Uv.Point (Qty units))

--- CrossMultiplication' instances ---
--------------------------------------

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Function Float (Vector2d (space1 @ units1))
      .><. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  VectorCurve2d{vc2x = x1, vc2y = y1} .><. VectorCurve2d{vc2x = x2, vc2y = y2} =
    curve1d (Expression.difference (Expression.product x1 y2) (Expression.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1))
      .><. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d{vs2x = x1, vs2y = y1} .><. VectorSurface2d{vs2x = x2, vs2y = y2} =
    surface1d (Expression.difference (Expression.product x1 y2) (Expression.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Float (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Function Float (Qty (units1 :*: units2))
  expression .><. vector = expression .><. constant @Float vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .><. vector = expression .><. constant @Uv.Point vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  vector .><. expression = constant @Float vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  vector .><. expression = constant @Uv.Point vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Float (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Function Float (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Uv.Point (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Function Uv.Point (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Function Float (Vector2d (space2 @ units)) =
      Function Float (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Function Uv.Point (Vector2d (space2 @ units)) =
      Function Uv.Point (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Function Float Float)
    (Function Float output)
    (Function Float output)
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
    (Function Uv.Point Float)
    (Function Float output)
    (Function Uv.Point output)
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
    (Function Float Uv.Point)
    (Function Uv.Point output)
    (Function Float output)
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
    (Function Uv.Point Uv.Point)
    (Function Uv.Point output)
    (Function Uv.Point output)
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
  zero :: Function input output

instance Zero Float (Qty units) where
  zero = constant Qty.zero

instance Zero Uv.Point (Qty units) where
  zero = constant Qty.zero

instance Zero Float (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero Uv.Point (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero Float (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

instance Zero Uv.Point (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

class Origin input output where
  origin :: Function input output

instance Origin Float (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Uv.Point (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Float (Point3d (space @ units)) where
  origin = constant Point3d.origin

instance Origin Uv.Point (Point3d (space @ units)) where
  origin = constant Point3d.origin

class Constant input output where
  constant :: output -> Function input output

instance Constant Float (Qty units) where
  constant qty = curve1d (Expression.constant qty)

instance Constant Uv.Point (Qty units) where
  constant qty = surface1d (Expression.constant qty)

instance Constant Float (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    vectorCurve2d (Expression.constant x) (Expression.constant y)

instance Constant Uv.Point (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    vectorSurface2d (Expression.constant x) (Expression.constant y)

instance Constant Float (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    vectorCurve3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Uv.Point (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    vectorSurface3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Float (Point2d (space @ units)) where
  constant (Point2d x y) =
    curve2d (Expression.constant x) (Expression.constant y)

instance Constant Uv.Point (Point2d (space @ units)) where
  constant (Point2d x y) =
    surface2d (Expression.constant x) (Expression.constant y)

instance Constant Float (Point3d (space @ units)) where
  constant (Point3d x y z) =
    curve3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Uv.Point (Point3d (space @ units)) where
  constant (Point3d x y z) =
    surface3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

class XY input output where
  xy ::
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input output

instance XY Float (Vector2d (space @ units)) where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = vectorCurve2d x y

instance XY Uv.Point (Vector2d (space @ units)) where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = vectorSurface2d x y

instance XY Float (Point2d (space @ units)) where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = curve2d x y

instance XY Uv.Point (Point2d (space @ units)) where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = surface2d x y

class XYZ input output where
  xyz ::
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input output

instance XYZ Float (Vector3d (space @ units)) where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = vectorCurve3d x y z

instance XYZ Uv.Point (Vector3d (space @ units)) where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = vectorSurface3d x y z

instance XYZ Float (Point3d (space @ units)) where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = curve3d x y z

instance XYZ Uv.Point (Point3d (space @ units)) where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = surface3d x y z

class XComponent input output where
  xComponent :: Function input output -> Function input (Qty (UnitsOf output))

class YComponent input output where
  yComponent :: Function input output -> Function input (Qty (UnitsOf output))

class ZComponent input output where
  zComponent :: Function input output -> Function input (Qty (UnitsOf output))

instance XComponent input (Vector2d (space @ units)) where
  xComponent VectorCurve2d{vc2x} = curve1d vc2x
  xComponent VectorSurface2d{vs2x} = surface1d vs2x

instance XComponent input (Vector3d (space @ units)) where
  xComponent VectorCurve3d{vc3x} = curve1d vc3x
  xComponent VectorSurface3d{vs3x} = surface1d vs3x

instance YComponent input (Vector2d (space @ units)) where
  yComponent VectorCurve2d{vc2y} = curve1d vc2y
  yComponent VectorSurface2d{vs2y} = surface1d vs2y

instance YComponent input (Vector3d (space @ units)) where
  yComponent VectorCurve3d{vc3y} = curve1d vc3y
  yComponent VectorSurface3d{vs3y} = surface1d vs3y

instance ZComponent input (Vector3d (space @ units)) where
  zComponent VectorCurve3d{vc3z} = curve1d vc3z
  zComponent VectorSurface3d{vs3z} = surface1d vs3z

class XCoordinate input output where
  xCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

class YCoordinate input output where
  yCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

class ZCoordinate input output where
  zCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

instance XCoordinate input (Point2d (space @ units)) where
  xCoordinate Curve2d{c2x} = curve1d c2x
  xCoordinate Surface2d{s2x} = surface1d s2x

instance XCoordinate input (Point3d (space @ units)) where
  xCoordinate Curve3d{c3x} = curve1d c3x
  xCoordinate Surface3d{s3x} = surface1d s3x

instance YCoordinate input (Point2d (space @ units)) where
  yCoordinate Curve2d{c2y} = curve1d c2y
  yCoordinate Surface2d{s2y} = surface1d s2y

instance YCoordinate input (Point3d (space @ units)) where
  yCoordinate Curve3d{c3y} = curve1d c3y
  yCoordinate Surface3d{s3y} = surface1d s3y

instance ZCoordinate input (Point3d (space @ units)) where
  zCoordinate Curve3d{c3z} = curve1d c3z
  zCoordinate Surface3d{s3z} = surface1d s3z

parameter :: Function Float Float
parameter = curve1d Expression.parameter

u :: Function Uv.Point Float
u = surface1d Expression.u

v :: Function Uv.Point Float
v = surface1d Expression.v

squared' :: Function input (Qty units) -> Function input (Qty (units :*: units))
squared' Curve1d{c1x} = curve1d (Expression.squared c1x)
squared' Surface1d{s1x} = surface1d (Expression.squared s1x)

squared ::
  Units.Squared units1 units2 =>
  Function input (Qty units1) ->
  Function input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Function input (Qty (units :*: units)) -> Function input (Qty units)
sqrt' Curve1d{c1x} = curve1d (Expression.sqrt c1x)
sqrt' Surface1d{s1x} = surface1d (Expression.sqrt s1x)

sqrt ::
  Units.Squared units1 units2 =>
  Function input (Qty units2) ->
  Function input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Function input Angle -> Function input Float
sin Curve1d{c1x} = curve1d (Expression.sin c1x)
sin Surface1d{s1x} = surface1d (Expression.sin s1x)

cos :: Function input Angle -> Function input Float
cos Curve1d{c1x} = curve1d (Expression.cos c1x)
cos Surface1d{s1x} = surface1d (Expression.cos s1x)

-----------------------
--- DIFFERENTIATION ---
-----------------------

class CurveDerivative expression derivative | expression -> derivative where
  curveDerivative :: expression -> derivative

class SurfaceDerivative expression derivative | expression -> derivative where
  surfaceDerivative :: Uv.Parameter -> expression -> derivative

instance
  CurveDerivative
    (Function Float (Qty units))
    (Function Float (Qty units))
  where
  curveDerivative = c1d

instance
  SurfaceDerivative
    (Function Uv.Point (Qty units))
    (Function Uv.Point (Qty units))
  where
  surfaceDerivative U = s1du
  surfaceDerivative V = s1dv

instance
  CurveDerivative
    (Function Float (Vector2d (space @ units)))
    (Function Float (Vector2d (space @ units)))
  where
  curveDerivative = vc2d

instance
  CurveDerivative
    (Function Float (Point2d (space @ units)))
    (Function Float (Vector2d (space @ units)))
  where
  curveDerivative = c2d

instance
  SurfaceDerivative
    (Function Uv.Point (Vector2d (space @ units)))
    (Function Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative U = vs2du
  surfaceDerivative V = vs2dv

instance
  SurfaceDerivative
    (Function Uv.Point (Point2d (space @ units)))
    (Function Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative U = s2du
  surfaceDerivative V = s2dv

instance
  CurveDerivative
    (Function Float (Vector3d (space @ units)))
    (Function Float (Vector3d (space @ units)))
  where
  curveDerivative = vc3d

instance
  CurveDerivative
    (Function Float (Point3d (space @ units)))
    (Function Float (Vector3d (space @ units)))
  where
  curveDerivative = c3d

instance
  SurfaceDerivative
    (Function Uv.Point (Vector3d (space @ units)))
    (Function Uv.Point (Vector3d (space @ units)))
  where
  surfaceDerivative U = vs3du
  surfaceDerivative V = vs3dv

instance
  SurfaceDerivative
    (Function Uv.Point (Point3d (space @ units)))
    (Function Uv.Point (Vector3d (space @ units)))
  where
  surfaceDerivative U = s3du
  surfaceDerivative V = s3dv

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
  opensolid_curve1d_value_function :: Expression.Ptr -> FunPtr Curve1dValueFunction

foreign import ccall unsafe "opensolid_curve1d_bounds_function"
  opensolid_curve1d_bounds_function :: Expression.Ptr -> FunPtr Curve1dBoundsFunction

foreign import ccall unsafe "opensolid_surface1d_value_function"
  opensolid_surface1d_value_function :: Expression.Ptr -> FunPtr Surface1dValueFunction

foreign import ccall unsafe "opensolid_surface1d_bounds_function"
  opensolid_surface1d_bounds_function :: Expression.Ptr -> FunPtr Surface1dBoundsFunction

foreign import ccall unsafe "opensolid_curve2d_value_function"
  opensolid_curve2d_value_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Curve2dValueFunction

foreign import ccall unsafe "opensolid_curve2d_bounds_function"
  opensolid_curve2d_bounds_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Curve2dBoundsFunction

foreign import ccall unsafe "opensolid_surface2d_value_function"
  opensolid_surface2d_value_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Surface2dValueFunction

foreign import ccall unsafe "opensolid_surface2d_bounds_function"
  opensolid_surface2d_bounds_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Surface2dBoundsFunction

foreign import ccall unsafe "opensolid_curve3d_value_function"
  opensolid_curve3d_value_function :: Expression.Ptr -> Expression.Ptr -> Expression.Ptr -> FunPtr Curve3dValueFunction

foreign import ccall unsafe "opensolid_curve3d_bounds_function"
  opensolid_curve3d_bounds_function :: Expression.Ptr -> Expression.Ptr -> Expression.Ptr -> FunPtr Curve3dBoundsFunction

foreign import ccall unsafe "opensolid_surface3d_value_function"
  opensolid_surface3d_value_function :: Expression.Ptr -> Expression.Ptr -> Expression.Ptr -> FunPtr Surface3dValueFunction

foreign import ccall unsafe "opensolid_surface3d_bounds_function"
  opensolid_surface3d_bounds_function :: Expression.Ptr -> Expression.Ptr -> Expression.Ptr -> FunPtr Surface3dBoundsFunction

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

class Value input output where
  value :: Function input output -> input -> output

class
  Bounds input output inputBounds outputBounds
    | input -> inputBounds
    , output -> outputBounds
  where
  bounds :: Function input output -> inputBounds -> outputBounds

instance Value Float (Qty units) where
  value Curve1d{c1v} (Qty tValue) = Qty (c1v tValue)

instance Bounds Float (Qty units) (Range Unitless) (Range units) where
  bounds Curve1d{c1b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 16
      c1b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty xLow) (Qty xHigh))

instance Value Uv.Point (Qty units) where
  value Surface1d{s1v} uvPoint = do
    let Point2d (Qty uValue) (Qty vValue) = uvPoint
    Qty (s1v uValue vValue)

instance Bounds Uv.Point (Qty units) Uv.Bounds (Range units) where
  bounds Surface1d{s1b} uvBounds =
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

instance Value Float (Point2d (space @ units)) where
  value Curve2d{c2v} (Qty tValue) =
    unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      c2v tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance Bounds Float (Point2d (space @ units)) (Range Unitless) (Bounds2d (space @ units)) where
  bounds Curve2d{c2b} tRange =
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

instance Value Uv.Point (Point2d (space @ units)) where
  value Surface2d{s2v} uvPoint =
    unsafeDupablePerformIO IO.do
      let Point2d (Qty uValue) (Qty vValue) = uvPoint
      outputs <- Alloc.mallocBytes 16
      s2v uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance Bounds Uv.Point (Point2d (space @ units)) Uv.Bounds (Bounds2d (space @ units)) where
  bounds Surface2d{s2b} uvBounds =
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
