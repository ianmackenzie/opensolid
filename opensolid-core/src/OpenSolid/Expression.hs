module OpenSolid.Expression
  ( Expression
  , Constant (constant)
  , Zero (zero)
  , Origin (origin)
  , XY (xy)
  , XYZ (xyz)
  , XComponent (xComponent)
  , YComponent (yComponent)
  , ZComponent (zComponent)
  , XCoordinate (xCoordinate)
  , YCoordinate (yCoordinate)
  , ZCoordinate (zCoordinate)
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
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds3d (Bounds3d (Bounds3d))
import OpenSolid.Bytecode.Ast (Ast1d, Ast2d, Ast3d)
import OpenSolid.Bytecode.Ast qualified as Ast
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.SurfaceParameter (UvBounds, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))

type role Expression nominal nominal

data Expression input output where
  Curve1d ::
    Ast1d Float ->
    ~(Curve1dValueFunction, Curve1dBoundsFunction) ->
    Expression Float (Qty units)
  Surface1d ::
    Ast1d UvPoint ->
    ~(Surface1dValueFunction, Surface1dBoundsFunction) ->
    Expression UvPoint (Qty units)
  Curve2d ::
    Ast2d Float ->
    ~(Curve2dValueFunction, Curve2dBoundsFunction) ->
    Expression Float (Point2d (space @ units))
  Surface2d ::
    Ast2d UvPoint ->
    ~(Surface2dValueFunction, Surface2dBoundsFunction) ->
    Expression UvPoint (Point2d (space @ units))
  VectorCurve2d ::
    Ast2d Float ->
    ~(Curve2dValueFunction, Curve2dBoundsFunction) ->
    Expression Float (Vector2d (space @ units))
  VectorSurface2d ::
    Ast2d UvPoint ->
    ~(Surface2dValueFunction, Surface2dBoundsFunction) ->
    Expression UvPoint (Vector2d (space @ units))
  Curve3d ::
    Ast3d Float ->
    ~(Curve3dValueFunction, Curve3dBoundsFunction) ->
    Expression Float (Point3d (space @ units))
  Surface3d ::
    Ast3d UvPoint ->
    ~(Surface3dValueFunction, Surface3dBoundsFunction) ->
    Expression UvPoint (Point3d (space @ units))
  VectorCurve3d ::
    Ast3d Float ->
    ~(Curve3dValueFunction, Curve3dBoundsFunction) ->
    Expression Float (Vector3d (space @ units))
  VectorSurface3d ::
    Ast3d UvPoint ->
    ~(Surface3dValueFunction, Surface3dBoundsFunction) ->
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

class XYZ input output units | output -> units where
  xyz ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input output

instance XYZ Float (Vector3d (space @ units)) units where
  xyz (Curve1d x _) (Curve1d y _) (Curve1d z _) = vectorCurve3d (Ast.xyz x y z)

instance XYZ UvPoint (Vector3d (space @ units)) units where
  xyz (Surface1d x _) (Surface1d y _) (Surface1d z _) = vectorSurface3d (Ast.xyz x y z)

instance XYZ Float (Point3d (space @ units)) units where
  xyz (Curve1d x _) (Curve1d y _) (Curve1d z _) = curve3d (Ast.xyz x y z)

instance XYZ UvPoint (Point3d (space @ units)) units where
  xyz (Surface1d x _) (Surface1d y _) (Surface1d z _) = surface3d (Ast.xyz x y z)

class XComponent input output units | output -> units where
  xComponent :: Expression input output -> Expression input (Qty units)

class YComponent input output units | output -> units where
  yComponent :: Expression input output -> Expression input (Qty units)

class ZComponent input output units | output -> units where
  zComponent :: Expression input output -> Expression input (Qty units)

instance XComponent input (Vector2d (space @ units)) units where
  xComponent (VectorCurve2d ast _) = curve1d (Ast.xComponent2d ast)
  xComponent (VectorSurface2d ast _) = surface1d (Ast.xComponent2d ast)

instance XComponent input (Vector3d (space @ units)) units where
  xComponent (VectorCurve3d ast _) = curve1d (Ast.xComponent3d ast)
  xComponent (VectorSurface3d ast _) = surface1d (Ast.xComponent3d ast)

instance YComponent input (Vector2d (space @ units)) units where
  yComponent (VectorCurve2d ast _) = curve1d (Ast.yComponent2d ast)
  yComponent (VectorSurface2d ast _) = surface1d (Ast.yComponent2d ast)

instance YComponent input (Vector3d (space @ units)) units where
  yComponent (VectorCurve3d ast _) = curve1d (Ast.yComponent3d ast)
  yComponent (VectorSurface3d ast _) = surface1d (Ast.yComponent3d ast)

instance ZComponent input (Vector3d (space @ units)) units where
  zComponent (VectorCurve3d ast _) = curve1d (Ast.zComponent3d ast)
  zComponent (VectorSurface3d ast _) = surface1d (Ast.zComponent3d ast)

class XCoordinate input output units | output -> units where
  xCoordinate :: Expression input output -> Expression input (Qty units)

class YCoordinate input output units | output -> units where
  yCoordinate :: Expression input output -> Expression input (Qty units)

class ZCoordinate input output units | output -> units where
  zCoordinate :: Expression input output -> Expression input (Qty units)

instance XCoordinate input (Point2d (space @ units)) units where
  xCoordinate (Curve2d ast _) = curve1d (Ast.xComponent2d ast)
  xCoordinate (Surface2d ast _) = surface1d (Ast.xComponent2d ast)

instance XCoordinate input (Point3d (space @ units)) units where
  xCoordinate (Curve3d ast _) = curve1d (Ast.xComponent3d ast)
  xCoordinate (Surface3d ast _) = surface1d (Ast.xComponent3d ast)

instance YCoordinate input (Point2d (space @ units)) units where
  yCoordinate (Curve2d ast _) = curve1d (Ast.yComponent2d ast)
  yCoordinate (Surface2d ast _) = surface1d (Ast.yComponent2d ast)

instance YCoordinate input (Point3d (space @ units)) units where
  yCoordinate (Curve3d ast _) = curve1d (Ast.yComponent3d ast)
  yCoordinate (Surface3d ast _) = surface1d (Ast.yComponent3d ast)

instance ZCoordinate input (Point3d (space @ units)) units where
  zCoordinate (Curve3d ast _) = curve1d (Ast.zComponent3d ast)
  zCoordinate (Surface3d ast _) = surface1d (Ast.zComponent3d ast)

t :: Expression Float Float
t = curve1d Ast.curveParameter

r :: Expression Float Float
r = constant @Float 1.0 - t

u :: Expression UvPoint Float
u = surface1d (Ast.surfaceParameter SurfaceParameter.U)

v :: Expression UvPoint Float
v = surface1d (Ast.surfaceParameter SurfaceParameter.V)

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

type Curve1dValueFunction = Float -> Float

type Curve1dBoundsFunction = Range Unitless -> Range Unitless

type Surface1dValueFunction = UvPoint -> Float

type Surface1dBoundsFunction = UvBounds -> Range Unitless

type Curve2dValueFunction = Float -> Vector2d Ast.Coordinates

type Curve2dBoundsFunction = Range Unitless -> VectorBounds2d Ast.Coordinates

type Surface2dValueFunction = UvPoint -> Vector2d Ast.Coordinates

type Surface2dBoundsFunction = UvBounds -> VectorBounds2d Ast.Coordinates

type Curve3dValueFunction = Float -> Vector3d Ast.Coordinates

type Curve3dBoundsFunction = Range Unitless -> VectorBounds3d Ast.Coordinates

type Surface3dValueFunction = UvPoint -> Vector3d Ast.Coordinates

type Surface3dBoundsFunction = UvBounds -> VectorBounds3d Ast.Coordinates

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
  evaluate (Curve1d _ (f, _)) tValue = Qty.coerce (f tValue)
  evaluateBounds (Curve1d _ (_, f)) tRange = Range.coerce (f tRange)

instance
  Evaluation
    UvPoint
    (Qty units)
    UvBounds
    (Range units)
  where
  evaluate (Surface1d _ (f, _)) uvPoint = Qty.coerce (f uvPoint)
  evaluateBounds (Surface1d _ (_, f)) uvBounds = Range.coerce (f uvBounds)

instance
  Evaluation
    Float
    (Point2d (space @ units))
    (Range Unitless)
    (Bounds2d (space @ units))
  where
  evaluate (Curve2d _ (f, _)) tValue = do
    let Vector2d x y = f tValue
    Point2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (Curve2d _ (_, f)) tRange = do
    let VectorBounds2d x y = f tRange
    Bounds2d (Range.coerce x) (Range.coerce y)

instance
  Evaluation
    UvPoint
    (Point2d (space @ units))
    UvBounds
    (Bounds2d (space @ units))
  where
  evaluate (Surface2d _ (f, _)) uvPoint = do
    let Vector2d x y = f uvPoint
    Point2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (Surface2d _ (_, f)) uvBounds = do
    let VectorBounds2d x y = f uvBounds
    Bounds2d (Range.coerce x) (Range.coerce y)

instance
  Evaluation
    Float
    (Vector2d (space @ units))
    (Range Unitless)
    (VectorBounds2d (space @ units))
  where
  evaluate (VectorCurve2d _ (f, _)) tValue = do
    let Vector2d x y = f tValue
    Vector2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (VectorCurve2d _ (_, f)) tRange = do
    let VectorBounds2d x y = f tRange
    VectorBounds2d (Range.coerce x) (Range.coerce y)

instance
  Evaluation
    UvPoint
    (Vector2d (space @ units))
    UvBounds
    (VectorBounds2d (space @ units))
  where
  evaluate (VectorSurface2d _ (f, _)) uvPoint = do
    let Vector2d x y = f uvPoint
    Vector2d (Qty.coerce x) (Qty.coerce y)

  evaluateBounds (VectorSurface2d _ (_, f)) uvBounds = do
    let VectorBounds2d x y = f uvBounds
    VectorBounds2d (Range.coerce x) (Range.coerce y)

instance
  Evaluation
    Float
    (Point3d (space @ units))
    (Range Unitless)
    (Bounds3d (space @ units))
  where
  evaluate (Curve3d _ (f, _)) tValue = do
    let Vector3d x y z = f tValue
    Point3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (Curve3d _ (_, f)) tRange = do
    let VectorBounds3d x y z = f tRange
    Bounds3d (Range.coerce x) (Range.coerce y) (Range.coerce z)

instance
  Evaluation
    UvPoint
    (Point3d (space @ units))
    UvBounds
    (Bounds3d (space @ units))
  where
  evaluate (Surface3d _ (f, _)) uvPoint = do
    let Vector3d x y z = f uvPoint
    Point3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (Surface3d _ (_, f)) uvBounds = do
    let VectorBounds3d x y z = f uvBounds
    Bounds3d (Range.coerce x) (Range.coerce y) (Range.coerce z)

instance
  Evaluation
    Float
    (Vector3d (space @ units))
    (Range Unitless)
    (VectorBounds3d (space @ units))
  where
  evaluate (VectorCurve3d _ (f, _)) tValue = do
    let Vector3d x y z = f tValue
    Vector3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (VectorCurve3d _ (_, f)) tRange = do
    let VectorBounds3d x y z = f tRange
    VectorBounds3d (Range.coerce x) (Range.coerce y) (Range.coerce z)

instance
  Evaluation
    UvPoint
    (Vector3d (space @ units))
    UvBounds
    (VectorBounds3d (space @ units))
  where
  evaluate (VectorSurface3d _ (f, _)) uvPoint = do
    let Vector3d x y z = f uvPoint
    Vector3d (Qty.coerce x) (Qty.coerce y) (Qty.coerce z)

  evaluateBounds (VectorSurface3d _ (_, f)) uvBounds = do
    let VectorBounds3d x y z = f uvBounds
    VectorBounds3d (Range.coerce x) (Range.coerce y) (Range.coerce z)
