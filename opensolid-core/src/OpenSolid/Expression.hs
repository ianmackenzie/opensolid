module OpenSolid.Expression
  ( Expression
  , debug
  , Constant (constant)
  , Zero (zero)
  , Origin (origin)
  , XY (xy)
  , RightwardForwardUpward (rightwardForwardUpward)
  , xComponent
  , yComponent
  , rightwardComponent
  , forwardComponent
  , upwardComponent
  , xCoordinate
  , yCoordinate
  , rightwardCoordinate
  , forwardCoordinate
  , upwardCoordinate
  , t
  , u
  , v
  , uv
  , sqrt
  , sqrt'
  , squared
  , squared'
  , sin
  , cos
  , SquaredMagnitude' (squaredMagnitude')
  , SquaredMagnitude (squaredMagnitude)
  , Magnitude (magnitude)
  , TransformBy (transformBy)
  , PlaceIn (placeIn)
  , RelativeTo (relativeTo)
  , PlaceOn (placeOn)
  , ProjectInto (projectInto)
  , bezierCurve
  , Evaluation (evaluate, evaluateBounds)
  , solveMonotonicSurfaceU
  , solveMonotonicSurfaceV
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bytecode.Ast (Ast1d, Ast2d, Ast3d)
import OpenSolid.Bytecode.Ast qualified as Ast
import OpenSolid.Bytecode.Evaluate (Compiled)
import OpenSolid.Bytecode.Evaluate qualified as Evaluate
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds3d (Bounds3d)
  , Point3d (Point3d)
  , Vector3d (Vector3d)
  , VectorBounds3d (VectorBounds3d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceParameter (UvBounds, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

type role Expression nominal nominal

data Expression input output where
  Curve1d ::
    Ast1d Float ->
    ~(Compiled Float Float) ->
    Expression Float (Qty units)
  Surface1d ::
    Ast1d UvPoint ->
    ~(Compiled UvPoint Float) ->
    Expression UvPoint (Qty units)
  Curve2d ::
    Ast2d Float ->
    ~(Compiled Float (Vector2d Ast.Coordinates)) ->
    Expression Float (Point2d (space @ units))
  Surface2d ::
    Ast2d UvPoint ->
    ~(Compiled UvPoint (Vector2d Ast.Coordinates)) ->
    Expression UvPoint (Point2d (space @ units))
  VectorCurve2d ::
    Ast2d Float ->
    ~(Compiled Float (Vector2d Ast.Coordinates)) ->
    Expression Float (Vector2d (space @ units))
  VectorSurface2d ::
    Ast2d UvPoint ->
    ~(Compiled UvPoint (Vector2d Ast.Coordinates)) ->
    Expression UvPoint (Vector2d (space @ units))
  Curve3d ::
    Ast3d Float ->
    ~(Compiled Float (Vector3d Ast.Coordinates)) ->
    Expression Float (Point3d (space @ units))
  Surface3d ::
    Ast3d UvPoint ->
    ~(Compiled UvPoint (Vector3d Ast.Coordinates)) ->
    Expression UvPoint (Point3d (space @ units))
  VectorCurve3d ::
    Ast3d Float ->
    ~(Compiled Float (Vector3d Ast.Coordinates)) ->
    Expression Float (Vector3d (space @ units))
  VectorSurface3d ::
    Ast3d UvPoint ->
    ~(Compiled UvPoint (Vector3d Ast.Coordinates)) ->
    Expression UvPoint (Vector3d (space @ units))

curve1d :: Ast1d Float -> Expression Float (Qty units)
curve1d ast = Curve1d ast (Ast.compileCurve1d ast)

surface1d :: Ast1d UvPoint -> Expression UvPoint (Qty units)
surface1d ast = Surface1d ast (Ast.compileSurface1d ast)

curve2d :: Ast2d Float -> Expression Float (Point2d (space @ units))
curve2d ast = Curve2d ast (Ast.compileCurve2d ast)

surface2d :: Ast2d UvPoint -> Expression UvPoint (Point2d (space @ units))
surface2d ast = Surface2d ast (Ast.compileSurface2d ast)

vectorCurve2d :: Ast2d Float -> Expression Float (Vector2d (space @ units))
vectorCurve2d ast = VectorCurve2d ast (Ast.compileCurve2d ast)

vectorSurface2d :: Ast2d UvPoint -> Expression UvPoint (Vector2d (space @ units))
vectorSurface2d ast = VectorSurface2d ast (Ast.compileSurface2d ast)

curve3d :: Ast3d Float -> Expression Float (Point3d (space @ units))
curve3d ast = Curve3d ast (Ast.compileCurve3d ast)

surface3d :: Ast3d UvPoint -> Expression UvPoint (Point3d (space @ units))
surface3d ast = Surface3d ast (Ast.compileSurface3d ast)

vectorCurve3d :: Ast3d Float -> Expression Float (Vector3d (space @ units))
vectorCurve3d ast = VectorCurve3d ast (Ast.compileCurve3d ast)

vectorSurface3d :: Ast3d UvPoint -> Expression UvPoint (Vector3d (space @ units))
vectorSurface3d ast = VectorSurface3d ast (Ast.compileSurface3d ast)

debug :: Expression input output -> Text
debug expression = case expression of
  Curve1d ast _ -> Ast.debugCurve1d ast
  Surface1d ast _ -> Ast.debugSurface1d ast
  Curve2d ast _ -> Ast.debugCurve2d ast
  Surface2d ast _ -> Ast.debugSurface2d ast
  VectorCurve2d ast _ -> Ast.debugCurve2d ast
  VectorSurface2d ast _ -> Ast.debugSurface2d ast
  Curve3d ast _ -> Ast.debugCurve3d ast
  Surface3d ast _ -> Ast.debugSurface3d ast
  VectorCurve3d ast _ -> Ast.debugCurve3d ast
  VectorSurface3d ast _ -> Ast.debugSurface3d ast

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
  coerce (Curve1d ast functions) = Curve1d ast functions
  coerce (Surface1d ast functions) = Surface1d ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point2d (space @ units1)))
    (Expression input2 (Point2d (space @ units2)))
  where
  coerce (Curve2d ast functions) = Curve2d ast functions
  coerce (Surface2d ast functions) = Surface2d ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Vector2d (space @ units2)))
  where
  coerce (VectorCurve2d ast functions) = VectorCurve2d ast functions
  coerce (VectorSurface2d ast functions) = VectorSurface2d ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point3d (space @ units1)))
    (Expression input2 (Point3d (space @ units2)))
  where
  coerce (Curve3d ast functions) = Curve3d ast functions
  coerce (Surface3d ast functions) = Surface3d ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector3d (space @ units1)))
    (Expression input2 (Vector3d (space @ units2)))
  where
  coerce (VectorCurve3d ast functions) = VectorCurve3d ast functions
  coerce (VectorSurface3d ast functions) = VectorSurface3d ast functions

----------------
--- NEGATION ---
----------------

instance Negation (Expression Float (Qty units)) where
  negate (Curve1d ast _) = curve1d -ast

instance Negation (Expression UvPoint (Qty units)) where
  negate (Surface1d ast _) = surface1d -ast

instance Negation (Expression Float (Vector2d (space @ units))) where
  negate (VectorCurve2d ast _) = vectorCurve2d -ast

instance Negation (Expression UvPoint (Vector2d (space @ units))) where
  negate (VectorSurface2d ast _) = vectorSurface2d -ast

instance Negation (Expression Float (Vector3d (space @ units))) where
  negate (VectorCurve3d ast _) = vectorCurve3d -ast

instance Negation (Expression UvPoint (Vector3d (space @ units))) where
  negate (VectorSurface3d ast _) = vectorSurface3d -ast

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
  Curve1d lhs _ + Curve1d rhs _ = curve1d (lhs + rhs)

instance
  units1 ~ units2 =>
  Addition
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units1))
  where
  Surface1d lhs _ + Surface1d rhs _ = surface1d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d lhs _ + VectorCurve2d rhs _ = vectorCurve2d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  VectorSurface2d lhs _ + VectorSurface2d rhs _ = vectorSurface2d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d lhs _ + VectorCurve3d rhs _ = vectorCurve3d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  VectorSurface3d lhs _ + VectorSurface3d rhs _ = vectorSurface3d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d lhs _ + VectorCurve2d rhs _ = curve2d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Point2d (space1 @ units1)))
  where
  Surface2d lhs _ + VectorSurface2d rhs _ = surface2d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d lhs _ + VectorCurve3d rhs _ = curve3d (lhs + rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Point3d (space1 @ units1)))
  where
  Surface3d lhs _ + VectorSurface3d rhs _ = surface3d (lhs + rhs)

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
  Curve1d lhs _ - Curve1d rhs _ = curve1d (lhs - rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty units1))
  where
  Surface1d lhs _ - Surface1d rhs _ = surface1d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d lhs _ - VectorCurve2d rhs _ = vectorCurve2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  VectorSurface2d lhs _ - VectorSurface2d rhs _ = vectorSurface2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d lhs _ - VectorCurve3d rhs _ = vectorCurve3d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  VectorSurface3d lhs _ - VectorSurface3d rhs _ = vectorSurface3d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d lhs _ - VectorCurve2d rhs _ = curve2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Point2d (space1 @ units1)))
  where
  Surface2d lhs _ - VectorSurface2d rhs _ = surface2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Point2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  Curve2d lhs _ - Curve2d rhs _ = vectorCurve2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2d (space1 @ units1)))
    (Expression UvPoint (Point2d (space2 @ units2)))
    (Expression UvPoint (Vector2d (space1 @ units1)))
  where
  Surface2d lhs _ - Surface2d rhs _ = vectorSurface2d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d lhs _ - VectorCurve3d rhs _ = curve3d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Point3d (space1 @ units1)))
  where
  Surface3d lhs _ - VectorSurface3d rhs _ = surface3d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Point3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  Curve3d lhs _ - Curve3d rhs _ = vectorCurve3d (lhs - rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point3d (space1 @ units1)))
    (Expression UvPoint (Point3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ units1)))
  where
  Surface3d lhs _ - Surface3d rhs _ = vectorSurface3d (lhs - rhs)

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
  Curve1d lhs _ .*. Curve1d rhs _ = curve1d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  Surface1d lhs _ .*. Surface1d rhs _ = surface1d (lhs * rhs)

--- Qty-Vector2d ---
--------------------

instance
  Multiplication'
    (Expression Float (Qty units1))
    (Expression Float (Vector2d (space @ units2)))
    (Expression Float (Vector2d (space @ (units1 :*: units2))))
  where
  Curve1d lhs _ .*. VectorCurve2d rhs _ = vectorCurve2d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector2d (space @ units2)))
    (Expression UvPoint (Vector2d (space @ (units1 :*: units2))))
  where
  Surface1d lhs _ .*. VectorSurface2d rhs _ = vectorSurface2d (lhs * rhs)

--- Vector2d-Qty ---
--------------------

instance
  Multiplication'
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ (units1 :*: units2))))
  where
  VectorCurve2d lhs _ .*. Curve1d rhs _ = vectorCurve2d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ (units1 :*: units2))))
  where
  VectorSurface2d lhs _ .*. Surface1d rhs _ = vectorSurface2d (lhs * rhs)

--- Qty-Vector3d ---
--------------------

instance
  Multiplication'
    (Expression Float (Qty units1))
    (Expression Float (Vector3d (space @ units2)))
    (Expression Float (Vector3d (space @ (units1 :*: units2))))
  where
  Curve1d lhs _ .*. VectorCurve3d rhs _ = vectorCurve3d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Vector3d (space @ units2)))
    (Expression UvPoint (Vector3d (space @ (units1 :*: units2))))
  where
  Surface1d lhs _ .*. VectorSurface3d rhs _ = vectorSurface3d (lhs * rhs)

--- Vector3d-Qty ---
--------------------

instance
  Multiplication'
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ (units1 :*: units2))))
  where
  VectorCurve3d lhs _ .*. Curve1d rhs _ = vectorCurve3d (lhs * rhs)

instance
  Multiplication'
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ (units1 :*: units2))))
  where
  VectorSurface3d lhs _ .*. Surface1d rhs _ = vectorSurface3d (lhs * rhs)

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
  Curve1d lhs _ ./. Curve1d rhs _ = curve1d (lhs / rhs)

instance
  Division'
    (Expression UvPoint (Qty units1))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Qty (units1 :/: units2)))
  where
  Surface1d lhs _ ./. Surface1d rhs _ = surface1d (lhs / rhs)

--- Vector2d-Qty ---
--------------------

instance
  Division'
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ (units1 :/: units2))))
  where
  VectorCurve2d lhs _ ./. Curve1d rhs _ = vectorCurve2d (lhs / rhs)

instance
  Division'
    (Expression UvPoint (Vector2d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector2d (space @ (units1 :/: units2))))
  where
  VectorSurface2d lhs _ ./. Surface1d rhs _ = vectorSurface2d (lhs / rhs)

--- Vector3d-Qty ---
--------------------

instance
  Division'
    (Expression Float (Vector3d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector3d (space @ (units1 :/: units2))))
  where
  VectorCurve3d lhs _ ./. Curve1d rhs _ = vectorCurve3d (lhs / rhs)

instance
  Division'
    (Expression UvPoint (Vector3d (space @ units1)))
    (Expression UvPoint (Qty units2))
    (Expression UvPoint (Vector3d (space @ (units1 :/: units2))))
  where
  VectorSurface3d lhs _ ./. Surface1d rhs _ = vectorSurface3d (lhs / rhs)

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
  VectorCurve2d lhs _ `dot'` VectorCurve2d rhs _ = curve1d (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface2d lhs _ `dot'` VectorSurface2d rhs _ = surface1d (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Qty (units1 :*: units2)))
  where
  VectorCurve3d lhs _ `dot'` VectorCurve3d rhs _ = curve1d (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface3d lhs _ `dot'` VectorSurface3d rhs _ = surface1d (lhs `dot` rhs)

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
  VectorCurve2d lhs _ `cross'` VectorCurve2d rhs _ = curve1d (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression UvPoint (Vector2d (space1 @ units1)))
    (Expression UvPoint (Vector2d (space2 @ units2)))
    (Expression UvPoint (Qty (units1 :*: units2)))
  where
  VectorSurface2d lhs _ `cross'` VectorSurface2d rhs _ = surface1d (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ (units1 :*: units2))))
  where
  VectorCurve3d lhs _ `cross'` VectorCurve3d rhs _ = vectorCurve3d (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression UvPoint (Vector3d (space1 @ units1)))
    (Expression UvPoint (Vector3d (space2 @ units2)))
    (Expression UvPoint (Vector3d (space1 @ (units1 :*: units2))))
  where
  VectorSurface3d lhs _ `cross'` VectorSurface3d rhs _ = vectorSurface3d (lhs `cross` rhs)

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Expression Float Float)
    (Expression Float output)
    (Expression Float output)
  where
  curve . Curve1d inner _ =
    case curve of
      Curve1d outer _ -> curve1d (outer . inner)
      Curve2d outer _ -> curve2d (outer . inner)
      VectorCurve2d outer _ -> vectorCurve2d (outer . inner)
      Curve3d outer _ -> curve3d (outer . inner)
      VectorCurve3d outer _ -> vectorCurve3d (outer . inner)

instance
  Composition
    (Expression UvPoint Float)
    (Expression Float output)
    (Expression UvPoint output)
  where
  curve . Surface1d inner _ =
    case curve of
      Curve1d outer _ -> surface1d (outer . inner)
      Curve2d outer _ -> surface2d (outer . inner)
      VectorCurve2d outer _ -> vectorSurface2d (outer . inner)
      Curve3d outer _ -> surface3d (outer . inner)
      VectorCurve3d outer _ -> vectorSurface3d (outer . inner)

instance
  Composition
    (Expression Float UvPoint)
    (Expression UvPoint output)
    (Expression Float output)
  where
  surface . Curve2d inner _ =
    case surface of
      Surface1d outer _ -> curve1d (outer . inner)
      Surface2d outer _ -> curve2d (outer . inner)
      VectorSurface2d outer _ -> vectorCurve2d (outer . inner)
      Surface3d outer _ -> curve3d (outer . inner)
      VectorSurface3d outer _ -> vectorCurve3d (outer . inner)

instance
  Composition
    (Expression UvPoint UvPoint)
    (Expression UvPoint output)
    (Expression UvPoint output)
  where
  surface . Surface2d inner _ =
    case surface of
      Surface1d outer _ -> surface1d (outer . inner)
      Surface2d outer _ -> surface2d (outer . inner)
      VectorSurface2d outer _ -> vectorSurface2d (outer . inner)
      Surface3d outer _ -> surface3d (outer . inner)
      VectorSurface3d outer _ -> vectorSurface3d (outer . inner)

-----------------
--- FUNCTIONS ---
-----------------

toVector2d :: Point2d (space @ units) -> Vector2d (space @ units)
toVector2d (Point2d x y) = Vector2d x y

toVector3d :: Point3d (space @ units) -> Vector3d (space @ units)
toVector3d (Point3d x y z) = Vector3d x y z

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
  constant value = curve1d (Ast.constant1d value)

instance Constant UvPoint (Qty units) where
  constant value = surface1d (Ast.constant1d value)

instance Constant Float (Vector2d (space @ units)) where
  constant value = vectorCurve2d (Ast.constant2d value)

instance Constant UvPoint (Vector2d (space @ units)) where
  constant value = vectorSurface2d (Ast.constant2d value)

instance Constant Float (Vector3d (space @ units)) where
  constant value = vectorCurve3d (Ast.constant3d value)

instance Constant UvPoint (Vector3d (space @ units)) where
  constant value = vectorSurface3d (Ast.constant3d value)

instance Constant Float (Point2d (space @ units)) where
  constant point = curve2d (Ast.constant2d (toVector2d point))

instance Constant UvPoint (Point2d (space @ units)) where
  constant point = surface2d (Ast.constant2d (toVector2d point))

instance Constant Float (Point3d (space @ units)) where
  constant point = curve3d (Ast.constant3d (toVector3d point))

instance Constant UvPoint (Point3d (space @ units)) where
  constant point = surface3d (Ast.constant3d (toVector3d point))

class XY input output units | output -> units where
  xy ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input output

instance XY Float (Vector2d (space @ units)) units where
  xy (Curve1d x _) (Curve1d y _) = vectorCurve2d (Ast.xy x y)

instance XY UvPoint (Vector2d (space @ units)) units where
  xy (Surface1d x _) (Surface1d y _) = vectorSurface2d (Ast.xy x y)

instance XY Float (Point2d (space @ units)) units where
  xy (Curve1d x _) (Curve1d y _) = curve2d (Ast.xy x y)

instance XY UvPoint (Point2d (space @ units)) units where
  xy (Surface1d x _) (Surface1d y _) = surface2d (Ast.xy x y)

class RightwardForwardUpward input output units | output -> units where
  rightwardForwardUpward ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input output

instance RightwardForwardUpward Float (Vector3d (space @ units)) units where
  rightwardForwardUpward (Curve1d vr _) (Curve1d vf _) (Curve1d vu _) =
    vectorCurve3d (Ast.rightwardForwardUpward vr vf vu)

instance RightwardForwardUpward UvPoint (Vector3d (space @ units)) units where
  rightwardForwardUpward (Surface1d vr _) (Surface1d vf _) (Surface1d vu _) =
    vectorSurface3d (Ast.rightwardForwardUpward vr vf vu)

instance RightwardForwardUpward Float (Point3d (space @ units)) units where
  rightwardForwardUpward (Curve1d pr _) (Curve1d pf _) (Curve1d pu _) =
    curve3d (Ast.rightwardForwardUpward pr pf pu)

instance RightwardForwardUpward UvPoint (Point3d (space @ units)) units where
  rightwardForwardUpward (Surface1d pr _) (Surface1d pf _) (Surface1d pu _) =
    surface3d (Ast.rightwardForwardUpward pr pf pu)

xComponent :: Expression input (Vector2d (space @ units)) -> Expression input (Qty units)
xComponent (VectorCurve2d ast _) = curve1d (Ast.xComponent ast)
xComponent (VectorSurface2d ast _) = surface1d (Ast.xComponent ast)

yComponent :: Expression input (Vector2d (space @ units)) -> Expression input (Qty units)
yComponent (VectorCurve2d ast _) = curve1d (Ast.yComponent ast)
yComponent (VectorSurface2d ast _) = surface1d (Ast.yComponent ast)

xCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
xCoordinate (Curve2d ast _) = curve1d (Ast.xComponent ast)
xCoordinate (Surface2d ast _) = surface1d (Ast.xComponent ast)

yCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
yCoordinate (Curve2d ast _) = curve1d (Ast.yComponent ast)
yCoordinate (Surface2d ast _) = surface1d (Ast.yComponent ast)

rightwardComponent :: Expression input (Vector3d (space @ units)) -> Expression input (Qty units)
rightwardComponent (VectorCurve3d ast _) = curve1d (Ast.rightwardComponent ast)
rightwardComponent (VectorSurface3d ast _) = surface1d (Ast.rightwardComponent ast)

forwardComponent :: Expression input (Vector3d (space @ units)) -> Expression input (Qty units)
forwardComponent (VectorCurve3d ast _) = curve1d (Ast.forwardComponent ast)
forwardComponent (VectorSurface3d ast _) = surface1d (Ast.forwardComponent ast)

upwardComponent :: Expression input (Vector3d (space @ units)) -> Expression input (Qty units)
upwardComponent (VectorCurve3d ast _) = curve1d (Ast.upwardComponent ast)
upwardComponent (VectorSurface3d ast _) = surface1d (Ast.upwardComponent ast)

rightwardCoordinate :: Expression input (Point3d (space @ units)) -> Expression input (Qty units)
rightwardCoordinate (Curve3d ast _) = curve1d (Ast.rightwardComponent ast)
rightwardCoordinate (Surface3d ast _) = surface1d (Ast.rightwardComponent ast)

forwardCoordinate :: Expression input (Point3d (space @ units)) -> Expression input (Qty units)
forwardCoordinate (Curve3d ast _) = curve1d (Ast.forwardComponent ast)
forwardCoordinate (Surface3d ast _) = surface1d (Ast.forwardComponent ast)

upwardCoordinate :: Expression input (Point3d (space @ units)) -> Expression input (Qty units)
upwardCoordinate (Curve3d ast _) = curve1d (Ast.upwardComponent ast)
upwardCoordinate (Surface3d ast _) = surface1d (Ast.upwardComponent ast)

t :: Expression Float Float
t = curve1d Ast.curveParameter

u :: Expression UvPoint Float
u = surface1d (Ast.surfaceParameter SurfaceParameter.U)

v :: Expression UvPoint Float
v = surface1d (Ast.surfaceParameter SurfaceParameter.V)

uv :: Expression UvPoint UvPoint
uv = surface2d Ast.surfaceParameters

squared' :: Expression input (Qty units) -> Expression input (Qty (units :*: units))
squared' (Curve1d ast _) = curve1d (Ast.squared ast)
squared' (Surface1d ast _) = surface1d (Ast.squared ast)

squared ::
  Units.Squared units1 units2 =>
  Expression input (Qty units1) ->
  Expression input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Expression input (Qty (units :*: units)) -> Expression input (Qty units)
sqrt' (Curve1d ast _) = curve1d (Ast.sqrt ast)
sqrt' (Surface1d ast _) = surface1d (Ast.sqrt ast)

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Qty units2) ->
  Expression input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Expression input Angle -> Expression input Float
sin (Curve1d ast _) = curve1d (Ast.sin ast)
sin (Surface1d ast _) = surface1d (Ast.sin ast)

cos :: Expression input Angle -> Expression input Float
cos (Curve1d ast _) = curve1d (Ast.cos ast)
cos (Surface1d ast _) = surface1d (Ast.cos ast)

class SquaredMagnitude' expression1 expression2 | expression1 -> expression2 where
  squaredMagnitude' :: expression1 -> expression2

instance
  SquaredMagnitude'
    (Expression input (Vector2d (space @ units)))
    (Expression input (Qty (units :*: units)))
  where
  squaredMagnitude' (VectorCurve2d ast _) = curve1d (Ast.squaredMagnitude2d ast)
  squaredMagnitude' (VectorSurface2d ast _) = surface1d (Ast.squaredMagnitude2d ast)

instance
  SquaredMagnitude'
    (Expression input (Vector3d (space @ units)))
    (Expression input (Qty (units :*: units)))
  where
  squaredMagnitude' (VectorCurve3d ast _) = curve1d (Ast.squaredMagnitude3d ast)
  squaredMagnitude' (VectorSurface3d ast _) = surface1d (Ast.squaredMagnitude3d ast)

class SquaredMagnitude expression1 expression2 | expression1 -> expression2 where
  squaredMagnitude :: expression1 -> expression2

instance
  Units.Squared units1 units2 =>
  SquaredMagnitude
    (Expression input (Vector2d (space @ units1)))
    (Expression input (Qty units2))
  where
  squaredMagnitude = Units.specialize . squaredMagnitude'

instance
  Units.Squared units1 units2 =>
  SquaredMagnitude
    (Expression input (Vector3d (space @ units1)))
    (Expression input (Qty units2))
  where
  squaredMagnitude = Units.specialize . squaredMagnitude'

class Magnitude expression1 expression2 | expression1 -> expression2 where
  magnitude :: expression1 -> expression2

instance
  Magnitude
    (Expression input (Vector2d (space @ units)))
    (Expression input (Qty units))
  where
  magnitude (VectorCurve2d ast _) = curve1d (Ast.magnitude2d ast)
  magnitude (VectorSurface2d ast _) = surface1d (Ast.magnitude2d ast)

instance
  Magnitude
    (Expression input (Vector3d (space @ units)))
    (Expression input (Qty units))
  where
  magnitude (VectorCurve3d ast _) = curve1d (Ast.magnitude3d ast)
  magnitude (VectorSurface3d ast _) = surface1d (Ast.magnitude3d ast)

class
  TransformBy transform expression1 expression2
    | transform expression1 -> expression2
  where
  transformBy :: transform -> expression1 -> expression2

instance
  space1 ~ space2 =>
  TransformBy
    (Transform2d tag (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Vector2d (space2 @ units2)))
  where
  transformBy transform (VectorCurve2d ast _) =
    vectorCurve2d (Ast.transformVector2d transform ast)
  transformBy transform (VectorSurface2d ast _) =
    vectorSurface2d (Ast.transformVector2d transform ast)

instance
  (space1 ~ space2, units1 ~ units2) =>
  TransformBy
    (Transform2d tag (space1 @ units1))
    (Expression input (Point2d (space2 @ units2)))
    (Expression input (Point2d (space2 @ units2)))
  where
  transformBy transform (Curve2d ast _) =
    curve2d (Ast.transformPoint2d transform ast)
  transformBy transform (Surface2d ast _) =
    surface2d (Ast.transformPoint2d transform ast)

instance
  space1 ~ space2 =>
  TransformBy
    (Transform3d tag (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Vector3d (space2 @ units2)))
  where
  transformBy transform (VectorCurve3d ast _) =
    vectorCurve3d (Ast.transformVector3d transform ast)
  transformBy transform (VectorSurface3d ast _) =
    vectorSurface3d (Ast.transformVector3d transform ast)

instance
  (space1 ~ space2, units1 ~ units2) =>
  TransformBy
    (Transform3d tag (space1 @ units1))
    (Expression input (Point3d (space2 @ units2)))
    (Expression input (Point3d (space2 @ units2)))
  where
  transformBy transform (Curve3d ast _) =
    curve3d (Ast.transformPoint3d transform ast)
  transformBy transform (Surface3d ast _) =
    surface3d (Ast.transformPoint3d transform ast)

class
  PlaceIn frame expression1 expression2
    | frame expression1 -> expression2
    , frame expression2 -> expression1
  where
  placeIn :: frame -> expression1 -> expression2

instance
  local1 ~ local2 =>
  PlaceIn
    (Basis2d global (Defines local1))
    (Expression input (Vector2d (local2 @ units)))
    (Expression input (Vector2d (global @ units)))
  where
  placeIn basis (VectorCurve2d ast _) = vectorCurve2d (Ast.placeVector2dIn basis ast)
  placeIn basis (VectorSurface2d ast _) = vectorSurface2d (Ast.placeVector2dIn basis ast)

instance
  (local1 ~ local2, units1 ~ units2) =>
  PlaceIn
    (Frame2d (global @ units1) (Defines local1))
    (Expression input (Point2d (local2 @ units2)))
    (Expression input (Point2d (global @ units2)))
  where
  placeIn frame (Curve2d ast _) = curve2d (Ast.placePoint2dIn frame ast)
  placeIn frame (Surface2d ast _) = surface2d (Ast.placePoint2dIn frame ast)

instance
  local1 ~ local2 =>
  PlaceIn
    (Basis3d global (Defines local1))
    (Expression input (Vector3d (local2 @ units)))
    (Expression input (Vector3d (global @ units)))
  where
  placeIn basis (VectorCurve3d ast _) = vectorCurve3d (Ast.placeVector3dIn basis ast)
  placeIn basis (VectorSurface3d ast _) = vectorSurface3d (Ast.placeVector3dIn basis ast)

instance
  (local1 ~ local2, units1 ~ units2) =>
  PlaceIn
    (Frame3d (global @ units1) (Defines local1))
    (Expression input (Point3d (local2 @ units2)))
    (Expression input (Point3d (global @ units2)))
  where
  placeIn frame (Curve3d ast _) = curve3d (Ast.placePoint3dIn frame ast)
  placeIn frame (Surface3d ast _) = surface3d (Ast.placePoint3dIn frame ast)

class
  RelativeTo frame expression1 expression2
    | frame expression1 -> expression2
    , frame expression2 -> expression1
  where
  relativeTo :: frame -> expression1 -> expression2

instance
  global1 ~ global2 =>
  RelativeTo
    (Basis2d global1 (Defines local))
    (Expression input (Vector2d (global2 @ units)))
    (Expression input (Vector2d (local @ units)))
  where
  relativeTo frame ast = placeIn (Basis2d.inverse frame) ast

instance
  (global1 ~ global2, units1 ~ units2) =>
  RelativeTo
    (Frame2d (global1 @ units1) (Defines local))
    (Expression input (Point2d (global2 @ units2)))
    (Expression input (Point2d (local @ units2)))
  where
  relativeTo frame ast = placeIn (Frame2d.inverse frame) ast

instance
  global1 ~ global2 =>
  RelativeTo
    (Basis3d global1 (Defines local))
    (Expression input (Vector3d (global2 @ units)))
    (Expression input (Vector3d (local @ units)))
  where
  relativeTo basis ast = placeIn (Basis3d.inverse basis) ast

instance
  (global1 ~ global2, units1 ~ units2) =>
  RelativeTo
    (Frame3d (global1 @ units1) (Defines local))
    (Expression input (Point3d (global2 @ units2)))
    (Expression input (Point3d (local @ units2)))
  where
  relativeTo frame ast = placeIn (Frame3d.inverse frame) ast

class PlaceOn plane expression1 expression2 | plane expression1 -> expression2 where
  placeOn :: plane -> expression1 -> expression2

instance
  local1 ~ local2 =>
  PlaceOn
    (PlanarBasis3d global (Defines local1))
    (Expression input (Vector2d (local2 @ units2)))
    (Expression input (Vector3d (global @ units2)))
  where
  placeOn basis (VectorCurve2d ast _) = vectorCurve3d (Ast.placeVector2dOn basis ast)
  placeOn basis (VectorSurface2d ast _) = vectorSurface3d (Ast.placeVector2dOn basis ast)

instance
  (local1 ~ local2, units1 ~ units2) =>
  PlaceOn
    (Plane3d (global @ units1) (Defines local1))
    (Expression input (Point2d (local2 @ units2)))
    (Expression input (Point3d (global @ units2)))
  where
  placeOn plane (Curve2d ast _) = curve3d (Ast.placePoint2dOn plane ast)
  placeOn plane (Surface2d ast _) = surface3d (Ast.placePoint2dOn plane ast)

class ProjectInto plane expression1 expression2 | plane expression1 -> expression2 where
  projectInto :: plane -> expression1 -> expression2

instance
  global1 ~ global2 =>
  ProjectInto
    (PlanarBasis3d global1 (Defines local))
    (Expression input (Vector3d (global2 @ units2)))
    (Expression input (Vector2d (local @ units2)))
  where
  projectInto basis (VectorCurve3d ast _) = vectorCurve2d (Ast.projectVector3dInto basis ast)
  projectInto basis (VectorSurface3d ast _) = vectorSurface2d (Ast.projectVector3dInto basis ast)

instance
  (global1 ~ global2, units1 ~ units2) =>
  ProjectInto
    (Plane3d (global1 @ units1) (Defines local))
    (Expression input (Point3d (global2 @ units2)))
    (Expression input (Point2d (local @ units2)))
  where
  projectInto plane (Curve3d ast _) = curve2d (Ast.projectPoint3dInto plane ast)
  projectInto plane (Surface3d ast _) = surface2d (Ast.projectPoint3dInto plane ast)

class BezierCurve output where
  bezierCurve :: NonEmpty output -> Expression Float output

instance BezierCurve (Qty units) where
  bezierCurve controlPoints = curve1d (Ast.bezierCurve1d controlPoints Ast.curveParameter)

instance BezierCurve (Vector2d (space @ units)) where
  bezierCurve controlPoints = vectorCurve2d (Ast.bezierCurve2d controlPoints Ast.curveParameter)

instance BezierCurve (Point2d (space @ units)) where
  bezierCurve controlPoints =
    curve2d (Ast.bezierCurve2d (NonEmpty.map toVector2d controlPoints) Ast.curveParameter)

instance BezierCurve (Vector3d (space @ units)) where
  bezierCurve controlPoints = vectorCurve3d (Ast.bezierCurve3d controlPoints Ast.curveParameter)

instance BezierCurve (Point3d (space @ units)) where
  bezierCurve controlPoints =
    curve3d (Ast.bezierCurve3d (NonEmpty.map toVector3d controlPoints) Ast.curveParameter)

-----------------
--- COMPILING ---
-----------------

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
    (Bounds Unitless)
    (Bounds units)
  where
  evaluate (Curve1d _ compiled) tValue =
    Qty.coerce (Evaluate.curve1dValue compiled tValue)
  evaluateBounds (Curve1d _ compiled) tBounds =
    Bounds.coerce (Evaluate.curve1dBounds compiled tBounds)

instance
  Evaluation
    UvPoint
    (Qty units)
    UvBounds
    (Bounds units)
  where
  evaluate (Surface1d _ compiled) uvPoint =
    Qty.coerce (Evaluate.surface1dValue compiled uvPoint)
  evaluateBounds (Surface1d _ compiled) uvBounds =
    Bounds.coerce (Evaluate.surface1dBounds compiled uvBounds)

instance
  Evaluation
    Float
    (Point2d (space @ units))
    (Bounds Unitless)
    (Bounds2d (space @ units))
  where
  evaluate (Curve2d _ compiled) tValue = do
    let Vector2d x y = Evaluate.curve2dValue compiled tValue
    Point2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (Curve2d _ compiled) tBounds = do
    let VectorBounds2d x y = Evaluate.curve2dBounds compiled tBounds
    Bounds2d (Bounds.coerce x) (Bounds.coerce y)

instance
  Evaluation
    UvPoint
    (Point2d (space @ units))
    UvBounds
    (Bounds2d (space @ units))
  where
  evaluate (Surface2d _ compiled) uvPoint = do
    let Vector2d x y = Evaluate.surface2dValue compiled uvPoint
    Point2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (Surface2d _ compiled) uvBounds = do
    let VectorBounds2d x y = Evaluate.surface2dBounds compiled uvBounds
    Bounds2d (Bounds.coerce x) (Bounds.coerce y)

instance
  Evaluation
    Float
    (Vector2d (space @ units))
    (Bounds Unitless)
    (VectorBounds2d (space @ units))
  where
  evaluate (VectorCurve2d _ compiled) tValue =
    Vector2d.coerce (Evaluate.curve2dValue compiled tValue)

  evaluateBounds (VectorCurve2d _ compiled) tBounds =
    VectorBounds2d.coerce (Evaluate.curve2dBounds compiled tBounds)

instance
  Evaluation
    UvPoint
    (Vector2d (space @ units))
    UvBounds
    (VectorBounds2d (space @ units))
  where
  evaluate (VectorSurface2d _ compiled) uvPoint =
    Vector2d.coerce (Evaluate.surface2dValue compiled uvPoint)

  evaluateBounds (VectorSurface2d _ compiled) uvBounds =
    VectorBounds2d.coerce (Evaluate.surface2dBounds compiled uvBounds)

instance
  Evaluation
    Float
    (Point3d (space @ units))
    (Bounds Unitless)
    (Bounds3d (space @ units))
  where
  evaluate (Curve3d _ compiled) tValue = do
    let Vector3d x y z = Evaluate.curve3dValue compiled tValue
    Point3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (Curve3d _ compiled) tBounds = do
    let VectorBounds3d x y z = Evaluate.curve3dBounds compiled tBounds
    Bounds3d (Bounds.coerce x) (Bounds.coerce y) (Bounds.coerce z)

instance
  Evaluation
    UvPoint
    (Point3d (space @ units))
    UvBounds
    (Bounds3d (space @ units))
  where
  evaluate (Surface3d _ compiled) uvPoint = do
    let Vector3d x y z = Evaluate.surface3dValue compiled uvPoint
    Point3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (Surface3d _ compiled) uvBounds = do
    let VectorBounds3d x y z = Evaluate.surface3dBounds compiled uvBounds
    Bounds3d (Bounds.coerce x) (Bounds.coerce y) (Bounds.coerce z)

instance
  Evaluation
    Float
    (Vector3d (space @ units))
    (Bounds Unitless)
    (VectorBounds3d (space @ units))
  where
  evaluate (VectorCurve3d _ compiled) tValue =
    Vector3d.coerce (Evaluate.curve3dValue compiled tValue)

  evaluateBounds (VectorCurve3d _ compiled) tBounds =
    VectorBounds3d.coerce (Evaluate.curve3dBounds compiled tBounds)

instance
  Evaluation
    UvPoint
    (Vector3d (space @ units))
    UvBounds
    (VectorBounds3d (space @ units))
  where
  evaluate (VectorSurface3d _ compiled) uvPoint =
    Vector3d.coerce (Evaluate.surface3dValue compiled uvPoint)

  evaluateBounds (VectorSurface3d _ compiled) uvBounds = do
    VectorBounds3d.coerce (Evaluate.surface3dBounds compiled uvBounds)

solveMonotonicSurfaceU ::
  Tolerance units =>
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Bounds Unitless ->
  Float ->
  Float
solveMonotonicSurfaceU (Surface1d _ function) (Surface1d _ derivative) uBounds vValue =
  Evaluate.solveMonotonicSurfaceU function derivative uBounds vValue

solveMonotonicSurfaceV ::
  Tolerance units =>
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Float ->
  Bounds Unitless ->
  Float
solveMonotonicSurfaceV (Surface1d _ function) (Surface1d _ derivative) uValue vBounds =
  Evaluate.solveMonotonicSurfaceV function derivative uValue vBounds
