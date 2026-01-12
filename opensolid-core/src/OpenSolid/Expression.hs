module OpenSolid.Expression
  ( Expression
  , debug
  , Constant (constant)
  , Zero (zero)
  , Origin (origin)
  , XY (xy)
  , xComponent
  , yComponent
  , xCoordinate
  , yCoordinate
  , t
  , u
  , v
  , uv
  , sqrt
  , sqrt_
  , squared
  , squared_
  , cubed
  , sin
  , cos
  , SquaredMagnitude' (squaredMagnitude_)
  , SquaredMagnitude (squaredMagnitude)
  , Magnitude (magnitude)
  , TransformBy (transformBy)
  , PlaceIn (placeIn)
  , RelativeTo (relativeTo)
  , On (on)
  , ProjectInto (projectInto)
  , bezierCurve
  , b00
  , b00d1
  , b00d2
  , b00d3
  , b01
  , b01d1
  , b01d2
  , b01d3
  , b02
  , b02d1
  , b02d2
  , b02d3
  , b10
  , b10d1
  , b10d2
  , b10d3
  , b11
  , b11d1
  , b11d2
  , b11d3
  , desingularized
  , Evaluation (evaluate, evaluateBounds)
  , solveMonotonicSurfaceU
  , solveMonotonicSurfaceV
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bytecode.Ast (Ast1D, Ast2D, Ast3D)
import OpenSolid.Bytecode.Ast qualified as Ast
import OpenSolid.Bytecode.Evaluate (Compiled)
import OpenSolid.Bytecode.Evaluate qualified as Evaluate
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2D (PositionBounds2D)
  , Bounds3D (PositionBounds3D)
  , Point2D (Position2D)
  , Point3D (Position3D)
  , Vector3D
  , VectorBounds3D
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D (VectorBounds2D)

type role Expression nominal nominal

data Expression input output where
  Curve1D ::
    Ast1D Number ->
    ~(Compiled Number Number) ->
    Expression Number (Quantity units)
  Surface1D ::
    Ast1D UvPoint ->
    ~(Compiled UvPoint Number) ->
    Expression UvPoint (Quantity units)
  Curve2D ::
    Ast2D Number ->
    ~(Compiled Number (Vector2D Unitless Ast.Space)) ->
    Expression Number (Point2D units space)
  Surface2D ::
    Ast2D UvPoint ->
    ~(Compiled UvPoint (Vector2D Unitless Ast.Space)) ->
    Expression UvPoint (Point2D units space)
  VectorCurve2D ::
    Ast2D Number ->
    ~(Compiled Number (Vector2D Unitless Ast.Space)) ->
    Expression Number (Vector2D units space)
  VectorSurface2D ::
    Ast2D UvPoint ->
    ~(Compiled UvPoint (Vector2D Unitless Ast.Space)) ->
    Expression UvPoint (Vector2D units space)
  Curve3D ::
    Ast3D Number ->
    ~(Compiled Number (Vector3D Unitless Ast.Space)) ->
    Expression Number (Point3D space)
  Surface3D ::
    Ast3D UvPoint ->
    ~(Compiled UvPoint (Vector3D Unitless Ast.Space)) ->
    Expression UvPoint (Point3D space)
  VectorCurve3D ::
    Ast3D Number ->
    ~(Compiled Number (Vector3D Unitless Ast.Space)) ->
    Expression Number (Vector3D units space)
  VectorSurface3D ::
    Ast3D UvPoint ->
    ~(Compiled UvPoint (Vector3D Unitless Ast.Space)) ->
    Expression UvPoint (Vector3D units space)

curve1D :: Ast1D Number -> Expression Number (Quantity units)
curve1D ast = Curve1D ast (Ast.compileCurve1D ast)

surface1D :: Ast1D UvPoint -> Expression UvPoint (Quantity units)
surface1D ast = Surface1D ast (Ast.compileSurface1D ast)

curve2D :: Ast2D Number -> Expression Number (Point2D units space)
curve2D ast = Curve2D ast (Ast.compileCurve2D ast)

surface2D :: Ast2D UvPoint -> Expression UvPoint (Point2D units space)
surface2D ast = Surface2D ast (Ast.compileSurface2D ast)

vectorCurve2D :: Ast2D Number -> Expression Number (Vector2D units space)
vectorCurve2D ast = VectorCurve2D ast (Ast.compileCurve2D ast)

vectorSurface2D :: Ast2D UvPoint -> Expression UvPoint (Vector2D units space)
vectorSurface2D ast = VectorSurface2D ast (Ast.compileSurface2D ast)

curve3D :: Ast3D Number -> Expression Number (Point3D space)
curve3D ast = Curve3D ast (Ast.compileCurve3D ast)

surface3D :: Ast3D UvPoint -> Expression UvPoint (Point3D space)
surface3D ast = Surface3D ast (Ast.compileSurface3D ast)

vectorCurve3D :: Ast3D Number -> Expression Number (Vector3D units space)
vectorCurve3D ast = VectorCurve3D ast (Ast.compileCurve3D ast)

vectorSurface3D :: Ast3D UvPoint -> Expression UvPoint (Vector3D units space)
vectorSurface3D ast = VectorSurface3D ast (Ast.compileSurface3D ast)

debug :: Expression input output -> Text
debug expression = case expression of
  Curve1D ast _ -> Ast.debugCurve1D ast
  Surface1D ast _ -> Ast.debugSurface1D ast
  Curve2D ast _ -> Ast.debugCurve2D ast
  Surface2D ast _ -> Ast.debugSurface2D ast
  VectorCurve2D ast _ -> Ast.debugCurve2D ast
  VectorSurface2D ast _ -> Ast.debugSurface2D ast
  Curve3D ast _ -> Ast.debugCurve3D ast
  Surface3D ast _ -> Ast.debugSurface3D ast
  VectorCurve3D ast _ -> Ast.debugCurve3D ast
  VectorSurface3D ast _ -> Ast.debugSurface3D ast

-------------
--- UNITS ---
-------------

instance HasUnits (Expression input (Quantity units)) units

instance HasUnits (Expression input (Vector2D units space)) units

instance HasUnits (Expression input (Vector3D units space)) units

instance HasUnits (Expression input (Point2D units space)) units

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Quantity units1))
    (Expression input2 (Quantity units2))
  where
  coerce (Curve1D ast functions) = Curve1D ast functions
  coerce (Surface1D ast functions) = Surface1D ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point2D units1 space))
    (Expression input2 (Point2D units2 space))
  where
  coerce (Curve2D ast functions) = Curve2D ast functions
  coerce (Surface2D ast functions) = Surface2D ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector2D units1 space))
    (Expression input2 (Vector2D units2 space))
  where
  coerce (VectorCurve2D ast functions) = VectorCurve2D ast functions
  coerce (VectorSurface2D ast functions) = VectorSurface2D ast functions

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector3D units1 space))
    (Expression input2 (Vector3D units2 space))
  where
  coerce (VectorCurve3D ast functions) = VectorCurve3D ast functions
  coerce (VectorSurface3D ast functions) = VectorSurface3D ast functions

----------------
--- NEGATION ---
----------------

instance Negation (Expression Number (Quantity units)) where
  negative (Curve1D ast _) = curve1D (negative ast)

instance Negation (Expression UvPoint (Quantity units)) where
  negative (Surface1D ast _) = surface1D (negative ast)

instance Negation (Expression Number (Vector2D units space)) where
  negative (VectorCurve2D ast _) = vectorCurve2D (negative ast)

instance Negation (Expression UvPoint (Vector2D units space)) where
  negative (VectorSurface2D ast _) = vectorSurface2D (negative ast)

instance Negation (Expression Number (Vector3D units space)) where
  negative (VectorCurve3D ast _) = vectorCurve3D (negative ast)

instance Negation (Expression UvPoint (Vector3D units space)) where
  negative (VectorSurface3D ast _) = vectorSurface3D (negative ast)

instance
  Multiplication
    Sign
    (Expression Number (Quantity units))
    (Expression Number (Quantity units))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Quantity units))
    (Expression UvPoint (Quantity units))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    (Expression Number (Quantity units))
    Sign
    (Expression Number (Quantity units))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

instance
  Multiplication
    (Expression UvPoint (Quantity units))
    Sign
    (Expression UvPoint (Quantity units))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

instance
  Multiplication
    Sign
    (Expression Number (Vector2D units space))
    (Expression Number (Vector2D units space))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Vector2D units space))
    (Expression UvPoint (Vector2D units space))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    (Expression Number (Vector2D units space))
    Sign
    (Expression Number (Vector2D units space))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

instance
  Multiplication
    (Expression UvPoint (Vector2D units space))
    Sign
    (Expression UvPoint (Vector2D units space))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

instance
  Multiplication
    Sign
    (Expression Number (Vector3D units space))
    (Expression Number (Vector3D units space))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    Sign
    (Expression UvPoint (Vector3D units space))
    (Expression UvPoint (Vector3D units space))
  where
  Positive .*. expression = expression
  Negative .*. expression = negative expression

instance
  Multiplication
    (Expression Number (Vector3D units space))
    Sign
    (Expression Number (Vector3D units space))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

instance
  Multiplication
    (Expression UvPoint (Vector3D units space))
    Sign
    (Expression UvPoint (Vector3D units space))
  where
  expression .*. Positive = expression
  expression .*. Negative = negative expression

----------------
--- ADDITION ---
----------------

instance
  units1 ~ units2 =>
  Addition
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity units1))
  where
  Curve1D lhs _ .+. Curve1D rhs _ = curve1D (lhs .+. rhs)

instance
  units1 ~ units2 =>
  Addition
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity units1))
  where
  Surface1D lhs _ .+. Surface1D rhs _ = surface1D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Vector2D units1 space1))
  where
  VectorCurve2D lhs _ .+. VectorCurve2D rhs _ = vectorCurve2D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Vector2D units1 space1))
  where
  VectorSurface2D lhs _ .+. VectorSurface2D rhs _ = vectorSurface2D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Vector3D units1 space1))
  where
  VectorCurve3D lhs _ .+. VectorCurve3D rhs _ = vectorCurve3D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Vector3D units1 space1))
  where
  VectorSurface3D lhs _ .+. VectorSurface3D rhs _ = vectorSurface3D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Number (Point2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Point2D units1 space1))
  where
  Curve2D lhs _ .+. VectorCurve2D rhs _ = curve2D (lhs .+. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression UvPoint (Point2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Point2D units1 space1))
  where
  Surface2D lhs _ .+. VectorSurface2D rhs _ = surface2D (lhs .+. rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Expression Number (Point3D space1))
    (Expression Number (Vector3D meters space2))
    (Expression Number (Point3D space1))
  where
  Curve3D lhs _ .+. VectorCurve3D rhs _ = curve3D (lhs .+. rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Expression UvPoint (Point3D space1))
    (Expression UvPoint (Vector3D meters space2))
    (Expression UvPoint (Point3D space1))
  where
  Surface3D lhs _ .+. VectorSurface3D rhs _ = surface3D (lhs .+. rhs)

-------------------
--- SUBTRACTION ---
-------------------

instance
  units1 ~ units2 =>
  Subtraction
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity units1))
  where
  Curve1D lhs _ .-. Curve1D rhs _ = curve1D (lhs .-. rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity units1))
  where
  Surface1D lhs _ .-. Surface1D rhs _ = surface1D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Vector2D units1 space1))
  where
  VectorCurve2D lhs _ .-. VectorCurve2D rhs _ = vectorCurve2D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Vector2D units1 space1))
  where
  VectorSurface2D lhs _ .-. VectorSurface2D rhs _ = vectorSurface2D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Vector3D units1 space1))
  where
  VectorCurve3D lhs _ .-. VectorCurve3D rhs _ = vectorCurve3D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Vector3D units1 space1))
  where
  VectorSurface3D lhs _ .-. VectorSurface3D rhs _ = vectorSurface3D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Number (Point2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Point2D units1 space1))
  where
  Curve2D lhs _ .-. VectorCurve2D rhs _ = curve2D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Point2D units1 space1))
  where
  Surface2D lhs _ .-. VectorSurface2D rhs _ = surface2D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Number (Point2D units1 space1))
    (Expression Number (Point2D units2 space2))
    (Expression Number (Vector2D units1 space1))
  where
  Curve2D lhs _ .-. Curve2D rhs _ = vectorCurve2D (lhs .-. rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression UvPoint (Point2D units1 space1))
    (Expression UvPoint (Point2D units2 space2))
    (Expression UvPoint (Vector2D units1 space1))
  where
  Surface2D lhs _ .-. Surface2D rhs _ = vectorSurface2D (lhs .-. rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Expression Number (Point3D space1))
    (Expression Number (Vector3D meters space2))
    (Expression Number (Point3D space1))
  where
  Curve3D lhs _ .-. VectorCurve3D rhs _ = curve3D (lhs .-. rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Expression UvPoint (Point3D space1))
    (Expression UvPoint (Vector3D meters space2))
    (Expression UvPoint (Point3D space1))
  where
  Surface3D lhs _ .-. VectorSurface3D rhs _ = surface3D (lhs .-. rhs)

instance
  space1 ~ space2 =>
  Subtraction
    (Expression Number (Point3D space1))
    (Expression Number (Point3D space2))
    (Expression Number (Vector3D Meters space1))
  where
  Curve3D lhs _ .-. Curve3D rhs _ = vectorCurve3D (lhs .-. rhs)

instance
  space1 ~ space2 =>
  Subtraction
    (Expression UvPoint (Point3D space1))
    (Expression UvPoint (Point3D space2))
    (Expression UvPoint (Vector3D Meters space1))
  where
  Surface3D lhs _ .-. Surface3D rhs _ = vectorSurface3D (lhs .-. rhs)

----------------------
--- MULTIPLICATION ---
----------------------

--------------------------------
--- Multiplication instances ---
--------------------------------

--- Quantity-Quantity ---
---------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity units3))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity units3))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

--- Quantity-Vector2D ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Number (Quantity units1))
    (Expression Number (Vector2D units2 space))
    (Expression Number (Vector2D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Vector2D units2 space))
    (Expression UvPoint (Vector2D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

--- Vector2D-Quantity ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Number (Vector2D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector2D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Vector2D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector2D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

--- Quantity-Vector3D ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Number (Quantity units1))
    (Expression Number (Vector3D units2 space))
    (Expression Number (Vector3D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Vector3D units2 space))
    (Expression UvPoint (Vector3D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

--- Vector3D-Quantity ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Number (Vector3D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector3D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression UvPoint (Vector3D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector3D units3 space))
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

---------------------------------
--- Multiplication_ instances ---
---------------------------------

--- Quantity-Quantity ---
---------------

instance
  Multiplication_
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity (units1 ?*? units2)))
  where
  Curve1D lhs _ ?*? Curve1D rhs _ = curve1D (lhs .*. rhs)

instance
  Multiplication_
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity (units1 ?*? units2)))
  where
  Surface1D lhs _ ?*? Surface1D rhs _ = surface1D (lhs .*. rhs)

--- Quantity-Vector2D ---
--------------------

instance
  Multiplication_
    (Expression Number (Quantity units1))
    (Expression Number (Vector2D units2 space))
    (Expression Number (Vector2D (units1 ?*? units2) space))
  where
  Curve1D lhs _ ?*? VectorCurve2D rhs _ = vectorCurve2D (lhs .*. rhs)

instance
  Multiplication_
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Vector2D units2 space))
    (Expression UvPoint (Vector2D (units1 ?*? units2) space))
  where
  Surface1D lhs _ ?*? VectorSurface2D rhs _ = vectorSurface2D (lhs .*. rhs)

--- Vector2D-Quantity ---
--------------------

instance
  Multiplication_
    (Expression Number (Vector2D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector2D (units1 ?*? units2) space))
  where
  VectorCurve2D lhs _ ?*? Curve1D rhs _ = vectorCurve2D (lhs .*. rhs)

instance
  Multiplication_
    (Expression UvPoint (Vector2D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector2D (units1 ?*? units2) space))
  where
  VectorSurface2D lhs _ ?*? Surface1D rhs _ = vectorSurface2D (lhs .*. rhs)

--- Quantity-Vector3D ---
--------------------

instance
  Multiplication_
    (Expression Number (Quantity units1))
    (Expression Number (Vector3D units2 space))
    (Expression Number (Vector3D (units1 ?*? units2) space))
  where
  Curve1D lhs _ ?*? VectorCurve3D rhs _ = vectorCurve3D (lhs .*. rhs)

instance
  Multiplication_
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Vector3D units2 space))
    (Expression UvPoint (Vector3D (units1 ?*? units2) space))
  where
  Surface1D lhs _ ?*? VectorSurface3D rhs _ = vectorSurface3D (lhs .*. rhs)

--- Vector3D-Quantity ---
--------------------

instance
  Multiplication_
    (Expression Number (Vector3D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector3D (units1 ?*? units2) space))
  where
  VectorCurve3D lhs _ ?*? Curve1D rhs _ = vectorCurve3D (lhs .*. rhs)

instance
  Multiplication_
    (Expression UvPoint (Vector3D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector3D (units1 ?*? units2) space))
  where
  VectorSurface3D lhs _ ?*? Surface1D rhs _ = vectorSurface3D (lhs .*. rhs)

----------------
--- DIVISION ---
----------------

--- Division instances ---
--------------------------

--- Quantity-Quantity ---
---------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity units3))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity units3))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

--- Vector2D-Quantity ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Number (Vector2D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector2D units3 space))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Vector2D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector2D units3 space))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

--- Vector3D-Quantity ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Number (Vector3D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector3D units3 space))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression UvPoint (Vector3D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector3D units3 space))
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

---------------------------
--- Division_ instances ---
---------------------------

--- Quantity-Quantity ---
---------------

instance
  Division_
    (Expression Number (Quantity units1))
    (Expression Number (Quantity units2))
    (Expression Number (Quantity (units1 ?/? units2)))
  where
  Curve1D lhs _ ?/? Curve1D rhs _ = curve1D (lhs ./. rhs)

instance
  Division_
    (Expression UvPoint (Quantity units1))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Quantity (units1 ?/? units2)))
  where
  Surface1D lhs _ ?/? Surface1D rhs _ = surface1D (lhs ./. rhs)

--- Vector2D-Quantity ---
--------------------

instance
  Division_
    (Expression Number (Vector2D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector2D (units1 ?/? units2) space))
  where
  VectorCurve2D lhs _ ?/? Curve1D rhs _ = vectorCurve2D (lhs ./. rhs)

instance
  Division_
    (Expression UvPoint (Vector2D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector2D (units1 ?/? units2) space))
  where
  VectorSurface2D lhs _ ?/? Surface1D rhs _ = vectorSurface2D (lhs ./. rhs)

--- Vector3D-Quantity ---
--------------------

instance
  Division_
    (Expression Number (Vector3D units1 space))
    (Expression Number (Quantity units2))
    (Expression Number (Vector3D (units1 ?/? units2) space))
  where
  VectorCurve3D lhs _ ?/? Curve1D rhs _ = vectorCurve3D (lhs ./. rhs)

instance
  Division_
    (Expression UvPoint (Vector3D units1 space))
    (Expression UvPoint (Quantity units2))
    (Expression UvPoint (Vector3D (units1 ?/? units2) space))
  where
  VectorSurface3D lhs _ ?/? Surface1D rhs _ = vectorSurface3D (lhs ./. rhs)

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Quantity units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Quantity units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Quantity units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Quantity units3))
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

--- DotMultiplication_ instances ---
------------------------------------

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Quantity (units1 ?*? units2)))
  where
  VectorCurve2D lhs _ `dot_` VectorCurve2D rhs _ = curve1D (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Quantity (units1 ?*? units2)))
  where
  VectorSurface2D lhs _ `dot_` VectorSurface2D rhs _ = surface1D (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Quantity (units1 ?*? units2)))
  where
  VectorCurve3D lhs _ `dot_` VectorCurve3D rhs _ = curve1D (lhs `dot` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Quantity (units1 ?*? units2)))
  where
  VectorSurface3D lhs _ `dot_` VectorSurface3D rhs _ = surface1D (lhs `dot` rhs)

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Quantity units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Quantity units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Vector3D units3 space1))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Vector3D units3 space1))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

--- CrossMultiplication_ instances ---
--------------------------------------

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Expression Number (Vector2D units1 space1))
    (Expression Number (Vector2D units2 space2))
    (Expression Number (Quantity (units1 ?*? units2)))
  where
  VectorCurve2D lhs _ `cross_` VectorCurve2D rhs _ = curve1D (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Expression UvPoint (Vector2D units1 space1))
    (Expression UvPoint (Vector2D units2 space2))
    (Expression UvPoint (Quantity (units1 ?*? units2)))
  where
  VectorSurface2D lhs _ `cross_` VectorSurface2D rhs _ = surface1D (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Expression Number (Vector3D units1 space1))
    (Expression Number (Vector3D units2 space2))
    (Expression Number (Vector3D (units1 ?*? units2) space1))
  where
  VectorCurve3D lhs _ `cross_` VectorCurve3D rhs _ = vectorCurve3D (lhs `cross` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Expression UvPoint (Vector3D units1 space1))
    (Expression UvPoint (Vector3D units2 space2))
    (Expression UvPoint (Vector3D (units1 ?*? units2) space1))
  where
  VectorSurface3D lhs _ `cross_` VectorSurface3D rhs _ = vectorSurface3D (lhs `cross` rhs)

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Expression Number Number)
    (Expression Number output)
    (Expression Number output)
  where
  curve `compose` Curve1D inner _ =
    case curve of
      Curve1D outer _ -> curve1D (outer `compose` inner)
      Curve2D outer _ -> curve2D (outer `compose` inner)
      VectorCurve2D outer _ -> vectorCurve2D (outer `compose` inner)
      Curve3D outer _ -> curve3D (outer `compose` inner)
      VectorCurve3D outer _ -> vectorCurve3D (outer `compose` inner)

instance
  Composition
    (Expression UvPoint Number)
    (Expression Number output)
    (Expression UvPoint output)
  where
  curve `compose` Surface1D inner _ =
    case curve of
      Curve1D outer _ -> surface1D (outer `compose` inner)
      Curve2D outer _ -> surface2D (outer `compose` inner)
      VectorCurve2D outer _ -> vectorSurface2D (outer `compose` inner)
      Curve3D outer _ -> surface3D (outer `compose` inner)
      VectorCurve3D outer _ -> vectorSurface3D (outer `compose` inner)

instance
  Composition
    (Expression Number UvPoint)
    (Expression UvPoint output)
    (Expression Number output)
  where
  surface `compose` Curve2D inner _ =
    case surface of
      Surface1D outer _ -> curve1D (outer `compose` inner)
      Surface2D outer _ -> curve2D (outer `compose` inner)
      VectorSurface2D outer _ -> vectorCurve2D (outer `compose` inner)
      Surface3D outer _ -> curve3D (outer `compose` inner)
      VectorSurface3D outer _ -> vectorCurve3D (outer `compose` inner)

instance
  Composition
    (Expression UvPoint UvPoint)
    (Expression UvPoint output)
    (Expression UvPoint output)
  where
  surface `compose` Surface2D inner _ =
    case surface of
      Surface1D outer _ -> surface1D (outer `compose` inner)
      Surface2D outer _ -> surface2D (outer `compose` inner)
      VectorSurface2D outer _ -> vectorSurface2D (outer `compose` inner)
      Surface3D outer _ -> surface3D (outer `compose` inner)
      VectorSurface3D outer _ -> vectorSurface3D (outer `compose` inner)

-----------------
--- FUNCTIONS ---
-----------------

class Zero input output where
  zero :: Expression input output

instance Zero Number (Quantity units) where
  zero = constant Quantity.zero

instance Zero UvPoint (Quantity units) where
  zero = constant Quantity.zero

instance Zero Number (Vector2D units space) where
  zero = constant Vector2D.zero

instance Zero UvPoint (Vector2D units space) where
  zero = constant Vector2D.zero

instance Zero Number (Vector3D units space) where
  zero = constant Vector3D.zero

instance Zero UvPoint (Vector3D units space) where
  zero = constant Vector3D.zero

class Origin input output where
  origin :: Expression input output

instance Origin Number (Point2D units space) where
  origin = constant Point2D.origin

instance Origin UvPoint (Point2D units space) where
  origin = constant Point2D.origin

class Constant input output where
  constant :: output -> Expression input output

instance Constant Number (Quantity units) where
  constant value = curve1D (Ast.constant1D value)

instance Constant UvPoint (Quantity units) where
  constant value = surface1D (Ast.constant1D value)

instance Constant Number (Vector2D units space) where
  constant value = vectorCurve2D (Ast.constant2D value)

instance Constant UvPoint (Vector2D units space) where
  constant value = vectorSurface2D (Ast.constant2D value)

instance Constant Number (Vector3D units space) where
  constant value = vectorCurve3D (Ast.constant3D value)

instance Constant UvPoint (Vector3D units space) where
  constant value = vectorSurface3D (Ast.constant3D value)

instance Constant Number (Point2D units space) where
  constant (Position2D p) = curve2D (Ast.constant2D p)

instance Constant UvPoint (Point2D units space) where
  constant (Position2D p) = surface2D (Ast.constant2D p)

instance Constant Number (Point3D space) where
  constant (Position3D p) = curve3D (Ast.constant3D p)

instance Constant UvPoint (Point3D space) where
  constant (Position3D p) = surface3D (Ast.constant3D p)

class XY input output units | output -> units where
  xy ::
    Expression input (Quantity units) ->
    Expression input (Quantity units) ->
    Expression input output

instance XY Number (Vector2D units space) units where
  xy (Curve1D x _) (Curve1D y _) = vectorCurve2D (Ast.xy x y)

instance XY UvPoint (Vector2D units space) units where
  xy (Surface1D x _) (Surface1D y _) = vectorSurface2D (Ast.xy x y)

instance XY Number (Point2D units space) units where
  xy (Curve1D x _) (Curve1D y _) = curve2D (Ast.xy x y)

instance XY UvPoint (Point2D units space) units where
  xy (Surface1D x _) (Surface1D y _) = surface2D (Ast.xy x y)

xComponent :: Expression input (Vector2D units space) -> Expression input (Quantity units)
xComponent (VectorCurve2D ast _) = curve1D (Ast.xComponent ast)
xComponent (VectorSurface2D ast _) = surface1D (Ast.xComponent ast)

yComponent :: Expression input (Vector2D units space) -> Expression input (Quantity units)
yComponent (VectorCurve2D ast _) = curve1D (Ast.yComponent ast)
yComponent (VectorSurface2D ast _) = surface1D (Ast.yComponent ast)

xCoordinate :: Expression input (Point2D units space) -> Expression input (Quantity units)
xCoordinate (Curve2D ast _) = curve1D (Ast.xComponent ast)
xCoordinate (Surface2D ast _) = surface1D (Ast.xComponent ast)

yCoordinate :: Expression input (Point2D units space) -> Expression input (Quantity units)
yCoordinate (Curve2D ast _) = curve1D (Ast.yComponent ast)
yCoordinate (Surface2D ast _) = surface1D (Ast.yComponent ast)

t :: Expression Number Number
t = curve1D Ast.curveParameter

u :: Expression UvPoint Number
u = surface1D (Ast.surfaceParameter SurfaceParameter.U)

v :: Expression UvPoint Number
v = surface1D (Ast.surfaceParameter SurfaceParameter.V)

uv :: Expression UvPoint UvPoint
uv = surface2D Ast.surfaceParameters

squared_ :: Expression input (Quantity units) -> Expression input (Quantity (units ?*? units))
squared_ (Curve1D ast _) = curve1D (Ast.squared ast)
squared_ (Surface1D ast _) = surface1D (Ast.squared ast)

squared ::
  Units.Squared units1 units2 =>
  Expression input (Quantity units1) ->
  Expression input (Quantity units2)
squared = Units.specialize . squared_

sqrt_ :: Expression input (Quantity (units ?*? units)) -> Expression input (Quantity units)
sqrt_ (Curve1D ast _) = curve1D (Ast.sqrt ast)
sqrt_ (Surface1D ast _) = surface1D (Ast.sqrt ast)

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Quantity units2) ->
  Expression input (Quantity units1)
sqrt = sqrt_ . Units.unspecialize

cubed :: Expression input Number -> Expression input Number
cubed (Curve1D ast _) = curve1D (Ast.cubed ast)
cubed (Surface1D ast _) = surface1D (Ast.cubed ast)

sin :: Expression input Angle -> Expression input Number
sin (Curve1D ast _) = curve1D (Ast.sin ast)
sin (Surface1D ast _) = surface1D (Ast.sin ast)

cos :: Expression input Angle -> Expression input Number
cos (Curve1D ast _) = curve1D (Ast.cos ast)
cos (Surface1D ast _) = surface1D (Ast.cos ast)

class SquaredMagnitude' expression1 expression2 | expression1 -> expression2 where
  squaredMagnitude_ :: expression1 -> expression2

instance
  SquaredMagnitude'
    (Expression input (Vector2D units space))
    (Expression input (Quantity (units ?*? units)))
  where
  squaredMagnitude_ (VectorCurve2D ast _) = curve1D (Ast.squaredMagnitude2D ast)
  squaredMagnitude_ (VectorSurface2D ast _) = surface1D (Ast.squaredMagnitude2D ast)

instance
  SquaredMagnitude'
    (Expression input (Vector3D units space))
    (Expression input (Quantity (units ?*? units)))
  where
  squaredMagnitude_ (VectorCurve3D ast _) = curve1D (Ast.squaredMagnitude3D ast)
  squaredMagnitude_ (VectorSurface3D ast _) = surface1D (Ast.squaredMagnitude3D ast)

class SquaredMagnitude expression1 expression2 | expression1 -> expression2 where
  squaredMagnitude :: expression1 -> expression2

instance
  Units.Squared units1 units2 =>
  SquaredMagnitude
    (Expression input (Vector2D units1 space))
    (Expression input (Quantity units2))
  where
  squaredMagnitude = Units.specialize . squaredMagnitude_

instance
  Units.Squared units1 units2 =>
  SquaredMagnitude
    (Expression input (Vector3D units1 space))
    (Expression input (Quantity units2))
  where
  squaredMagnitude = Units.specialize . squaredMagnitude_

class Magnitude expression1 expression2 | expression1 -> expression2 where
  magnitude :: expression1 -> expression2

instance
  Magnitude
    (Expression input (Vector2D units space))
    (Expression input (Quantity units))
  where
  magnitude (VectorCurve2D ast _) = curve1D (Ast.magnitude2D ast)
  magnitude (VectorSurface2D ast _) = surface1D (Ast.magnitude2D ast)

instance
  Magnitude
    (Expression input (Vector3D units space))
    (Expression input (Quantity units))
  where
  magnitude (VectorCurve3D ast _) = curve1D (Ast.magnitude3D ast)
  magnitude (VectorSurface3D ast _) = surface1D (Ast.magnitude3D ast)

class
  TransformBy transform expression1 expression2
    | transform expression1 -> expression2
  where
  transformBy :: transform -> expression1 -> expression2

instance
  space1 ~ space2 =>
  TransformBy
    (Transform2D tag units1 space1)
    (Expression input (Vector2D units2 space2))
    (Expression input (Vector2D units2 space2))
  where
  transformBy transform (VectorCurve2D ast _) =
    vectorCurve2D (Ast.transformVector2D transform ast)
  transformBy transform (VectorSurface2D ast _) =
    vectorSurface2D (Ast.transformVector2D transform ast)

instance
  (space1 ~ space2, units1 ~ units2) =>
  TransformBy
    (Transform2D tag units1 space1)
    (Expression input (Point2D units2 space2))
    (Expression input (Point2D units2 space2))
  where
  transformBy transform (Curve2D ast _) =
    curve2D (Ast.transformPoint2D transform ast)
  transformBy transform (Surface2D ast _) =
    surface2D (Ast.transformPoint2D transform ast)

instance
  space1 ~ space2 =>
  TransformBy
    (Transform3D tag space1)
    (Expression input (Vector3D units space2))
    (Expression input (Vector3D units space2))
  where
  transformBy transform (VectorCurve3D ast _) =
    vectorCurve3D (Ast.transformVector3D transform ast)
  transformBy transform (VectorSurface3D ast _) =
    vectorSurface3D (Ast.transformVector3D transform ast)

instance
  space1 ~ space2 =>
  TransformBy
    (Transform3D tag space1)
    (Expression input (Point3D space2))
    (Expression input (Point3D space2))
  where
  transformBy transform (Curve3D ast _) =
    curve3D (Ast.transformPoint3D transform ast)
  transformBy transform (Surface3D ast _) =
    surface3D (Ast.transformPoint3D transform ast)

class
  PlaceIn frame expression1 expression2
    | frame expression1 -> expression2
    , frame expression2 -> expression1
  where
  placeIn :: frame -> expression1 -> expression2

instance
  local1 ~ local2 =>
  PlaceIn
    (Frame2D frameUnits global local1)
    (Expression input (Vector2D units local2))
    (Expression input (Vector2D units global))
  where
  placeIn frame (VectorCurve2D ast _) = vectorCurve2D (Ast.placeVector2DIn frame ast)
  placeIn frame (VectorSurface2D ast _) = vectorSurface2D (Ast.placeVector2DIn frame ast)

instance
  (local1 ~ local2, units1 ~ units2) =>
  PlaceIn
    (Frame2D units1 global local1)
    (Expression input (Point2D units2 local2))
    (Expression input (Point2D units2 global))
  where
  placeIn frame (Curve2D ast _) = curve2D (Ast.placePoint2DIn frame ast)
  placeIn frame (Surface2D ast _) = surface2D (Ast.placePoint2DIn frame ast)

instance
  local1 ~ local2 =>
  PlaceIn
    (Frame3D global local1)
    (Expression input (Vector3D units local2))
    (Expression input (Vector3D units global))
  where
  placeIn frame (VectorCurve3D ast _) = vectorCurve3D (Ast.placeVector3dIn frame ast)
  placeIn frame (VectorSurface3D ast _) = vectorSurface3D (Ast.placeVector3dIn frame ast)

instance
  local1 ~ local2 =>
  PlaceIn
    (Frame3D global local1)
    (Expression input (Point3D local2))
    (Expression input (Point3D global))
  where
  placeIn frame (Curve3D ast _) = curve3D (Ast.placePoint3dIn frame ast)
  placeIn frame (Surface3D ast _) = surface3D (Ast.placePoint3dIn frame ast)

class
  RelativeTo frame expression1 expression2
    | frame expression1 -> expression2
    , frame expression2 -> expression1
  where
  relativeTo :: frame -> expression1 -> expression2

instance
  global1 ~ global2 =>
  RelativeTo
    (Frame2D frameUnits global1 local)
    (Expression input (Vector2D units global2))
    (Expression input (Vector2D units local))
  where
  relativeTo frame ast = placeIn (Frame2D.inverse frame) ast

instance
  (global1 ~ global2, units1 ~ units2) =>
  RelativeTo
    (Frame2D units1 global1 local)
    (Expression input (Point2D units2 global2))
    (Expression input (Point2D units2 local))
  where
  relativeTo frame ast = placeIn (Frame2D.inverse frame) ast

instance
  global1 ~ global2 =>
  RelativeTo
    (Frame3D global1 local)
    (Expression input (Vector3D units global2))
    (Expression input (Vector3D units local))
  where
  relativeTo frame ast = placeIn (Frame3D.inverse frame) ast

instance
  global1 ~ global2 =>
  RelativeTo
    (Frame3D global1 local)
    (Expression input (Point3D global2))
    (Expression input (Point3D local))
  where
  relativeTo frame ast = placeIn (Frame3D.inverse frame) ast

class On plane expression1 expression2 | plane expression1 -> expression2 where
  on :: plane -> expression1 -> expression2

instance
  local1 ~ local2 =>
  On
    (Plane3D global local1)
    (Expression input (Vector2D units local2))
    (Expression input (Vector3D units global))
  where
  on plane (VectorCurve2D ast _) = vectorCurve3D (Ast.placeVector2DOn plane ast)
  on plane (VectorSurface2D ast _) = vectorSurface3D (Ast.placeVector2DOn plane ast)

instance
  (local1 ~ local2, meters ~ Meters) =>
  On
    (Plane3D global local1)
    (Expression input (Point2D meters local2))
    (Expression input (Point3D global))
  where
  on plane (Curve2D ast _) = curve3D (Ast.placePoint2DOn plane ast)
  on plane (Surface2D ast _) = surface3D (Ast.placePoint2DOn plane ast)

class ProjectInto plane expression1 expression2 | plane expression1 -> expression2 where
  projectInto :: plane -> expression1 -> expression2

instance
  global1 ~ global2 =>
  ProjectInto
    (Plane3D global1 local)
    (Expression input (Vector3D units global2))
    (Expression input (Vector2D units local))
  where
  projectInto plane (VectorCurve3D ast _) = vectorCurve2D (Ast.projectVector3dInto plane ast)
  projectInto plane (VectorSurface3D ast _) = vectorSurface2D (Ast.projectVector3dInto plane ast)

instance
  (global1 ~ global2, meters ~ Meters) =>
  ProjectInto
    (Plane3D global1 local)
    (Expression input (Point3D global2))
    (Expression input (Point2D meters local))
  where
  projectInto plane (Curve3D ast _) = curve2D (Ast.projectPoint3dInto plane ast)
  projectInto plane (Surface3D ast _) = surface2D (Ast.projectPoint3dInto plane ast)

class BezierCurve output where
  bezierCurve :: NonEmpty output -> Expression Number output

instance BezierCurve (Quantity units) where
  bezierCurve controlPoints = curve1D (Ast.bezierCurve1D controlPoints)

instance BezierCurve (Vector2D units space) where
  bezierCurve controlPoints = vectorCurve2D (Ast.bezierCurve2D controlPoints)

instance BezierCurve (Point2D units space) where
  bezierCurve controlPoints = curve2D (Ast.bezierCurve2D (Data.Coerce.coerce controlPoints))

instance BezierCurve (Vector3D units space) where
  bezierCurve controlPoints = vectorCurve3D (Ast.bezierCurve3D controlPoints)

instance BezierCurve (Point3D space) where
  bezierCurve controlPoints = curve3D (Ast.bezierCurve3D (Data.Coerce.coerce controlPoints))

b00 :: Expression Number Number
b00 = curve1D Ast.b00

b00d1 :: Expression Number Number
b00d1 = curve1D Ast.b00d1

b00d2 :: Expression Number Number
b00d2 = curve1D Ast.b00d2

b00d3 :: Expression Number Number
b00d3 = curve1D Ast.b00d3

b01 :: Expression Number Number
b01 = curve1D Ast.b01

b01d1 :: Expression Number Number
b01d1 = curve1D Ast.b01d1

b01d2 :: Expression Number Number
b01d2 = curve1D Ast.b01d2

b01d3 :: Expression Number Number
b01d3 = curve1D Ast.b01d3

b02 :: Expression Number Number
b02 = curve1D Ast.b02

b02d1 :: Expression Number Number
b02d1 = curve1D Ast.b02d1

b02d2 :: Expression Number Number
b02d2 = curve1D Ast.b02d2

b02d3 :: Expression Number Number
b02d3 = curve1D Ast.b02d3

b10 :: Expression Number Number
b10 = curve1D Ast.b10

b10d1 :: Expression Number Number
b10d1 = curve1D Ast.b10d1

b10d2 :: Expression Number Number
b10d2 = curve1D Ast.b10d2

b10d3 :: Expression Number Number
b10d3 = curve1D Ast.b10d3

b11 :: Expression Number Number
b11 = curve1D Ast.b11

b11d1 :: Expression Number Number
b11d1 = curve1D Ast.b11d1

b11d2 :: Expression Number Number
b11d2 = curve1D Ast.b11d2

b11d3 :: Expression Number Number
b11d3 = curve1D Ast.b11d3

desingularized ::
  Expression input Number ->
  Expression input output ->
  Expression input output ->
  Expression input output ->
  Expression input output
desingularized
  (Curve1D parameter _)
  (Curve1D left _)
  (Curve1D middle _)
  (Curve1D right _) =
    curve1D (Ast.desingularized1D parameter left middle right)
desingularized
  (Surface1D parameter _)
  (Surface1D left _)
  (Surface1D middle _)
  (Surface1D right _) =
    surface1D (Ast.desingularized1D parameter left middle right)
desingularized
  (Curve1D parameter _)
  (Curve2D left _)
  (Curve2D middle _)
  (Curve2D right _) =
    curve2D (Ast.desingularized2D parameter left middle right)
desingularized
  (Surface1D parameter _)
  (Surface2D left _)
  (Surface2D middle _)
  (Surface2D right _) =
    surface2D (Ast.desingularized2D parameter left middle right)
desingularized
  (Curve1D parameter _)
  (VectorCurve2D left _)
  (VectorCurve2D middle _)
  (VectorCurve2D right _) =
    vectorCurve2D (Ast.desingularized2D parameter left middle right)
desingularized
  (Surface1D parameter _)
  (VectorSurface2D left _)
  (VectorSurface2D middle _)
  (VectorSurface2D right _) =
    vectorSurface2D (Ast.desingularized2D parameter left middle right)
desingularized
  (Curve1D parameter _)
  (Curve3D left _)
  (Curve3D middle _)
  (Curve3D right _) =
    curve3D (Ast.desingularized3D parameter left middle right)
desingularized
  (Surface1D parameter _)
  (Surface3D left _)
  (Surface3D middle _)
  (Surface3D right _) =
    surface3D (Ast.desingularized3D parameter left middle right)
desingularized
  (Curve1D parameter _)
  (VectorCurve3D left _)
  (VectorCurve3D middle _)
  (VectorCurve3D right _) =
    vectorCurve3D (Ast.desingularized3D parameter left middle right)
desingularized
  (Surface1D parameter _)
  (VectorSurface3D left _)
  (VectorSurface3D middle _)
  (VectorSurface3D right _) =
    vectorSurface3D (Ast.desingularized3D parameter left middle right)

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
    Number
    (Quantity units)
    (Bounds Unitless)
    (Bounds units)
  where
  evaluate (Curve1D _ compiled) tValue = Evaluate.curve1dValue compiled tValue
  evaluateBounds (Curve1D _ compiled) tBounds = Evaluate.curve1dBounds compiled tBounds

instance
  Evaluation
    UvPoint
    (Quantity units)
    UvBounds
    (Bounds units)
  where
  evaluate (Surface1D _ compiled) uvPoint = Evaluate.surface1dValue compiled uvPoint
  evaluateBounds (Surface1D _ compiled) uvBounds = Evaluate.surface1dBounds compiled uvBounds

instance
  Evaluation
    Number
    (Point2D units space)
    (Bounds Unitless)
    (Bounds2D units space)
  where
  evaluate (Curve2D _ compiled) tValue =
    Position2D (Evaluate.curve2dValue compiled tValue)

  evaluateBounds (Curve2D _ compiled) tBounds =
    PositionBounds2D (Evaluate.curve2dBounds compiled tBounds)

instance
  Evaluation
    UvPoint
    (Point2D units space)
    UvBounds
    (Bounds2D units space)
  where
  evaluate (Surface2D _ compiled) uvPoint =
    Position2D (Evaluate.surface2dValue compiled uvPoint)

  evaluateBounds (Surface2D _ compiled) uvBounds =
    PositionBounds2D (Evaluate.surface2dBounds compiled uvBounds)

instance
  Evaluation
    Number
    (Vector2D units space)
    (Bounds Unitless)
    (VectorBounds2D units space)
  where
  evaluate (VectorCurve2D _ compiled) tValue = Evaluate.curve2dValue compiled tValue
  evaluateBounds (VectorCurve2D _ compiled) tBounds = Evaluate.curve2dBounds compiled tBounds

instance
  Evaluation
    UvPoint
    (Vector2D units space)
    UvBounds
    (VectorBounds2D units space)
  where
  evaluate (VectorSurface2D _ compiled) uvPoint = Evaluate.surface2dValue compiled uvPoint
  evaluateBounds (VectorSurface2D _ compiled) uvBounds = Evaluate.surface2dBounds compiled uvBounds

instance
  Evaluation
    Number
    (Point3D space)
    (Bounds Unitless)
    (Bounds3D space)
  where
  evaluate (Curve3D _ compiled) tValue =
    Position3D (Evaluate.curve3dValue compiled tValue)

  evaluateBounds (Curve3D _ compiled) tBounds =
    PositionBounds3D (Evaluate.curve3dBounds compiled tBounds)

instance
  Evaluation
    UvPoint
    (Point3D space)
    UvBounds
    (Bounds3D space)
  where
  evaluate (Surface3D _ compiled) uvPoint =
    Position3D (Evaluate.surface3dValue compiled uvPoint)

  evaluateBounds (Surface3D _ compiled) uvBounds =
    PositionBounds3D (Evaluate.surface3dBounds compiled uvBounds)

instance
  Evaluation
    Number
    (Vector3D units space)
    (Bounds Unitless)
    (VectorBounds3D units space)
  where
  evaluate (VectorCurve3D _ compiled) tValue = Evaluate.curve3dValue compiled tValue
  evaluateBounds (VectorCurve3D _ compiled) tBounds = Evaluate.curve3dBounds compiled tBounds

instance
  Evaluation
    UvPoint
    (Vector3D units space)
    UvBounds
    (VectorBounds3D units space)
  where
  evaluate (VectorSurface3D _ compiled) uvPoint = Evaluate.surface3dValue compiled uvPoint
  evaluateBounds (VectorSurface3D _ compiled) uvBounds = Evaluate.surface3dBounds compiled uvBounds

solveMonotonicSurfaceU ::
  Tolerance units =>
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Bounds Unitless ->
  Number ->
  Number
solveMonotonicSurfaceU (Surface1D _ function) (Surface1D _ derivative) uBounds vValue =
  Evaluate.solveMonotonicSurfaceU function derivative uBounds vValue

solveMonotonicSurfaceV ::
  Tolerance units =>
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Number ->
  Bounds Unitless ->
  Number
solveMonotonicSurfaceV (Surface1D _ function) (Surface1D _ derivative) uValue vBounds =
  Evaluate.solveMonotonicSurfaceV function derivative uValue vBounds
