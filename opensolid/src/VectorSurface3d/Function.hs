-- Needed for 'Surface1d.Function * Vector3d = Function'
-- and 'Vector3d * Surface1d.Function = Function' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Surface1d.Function or Vector3d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorSurface3d.Function
  ( Function (Constant)
  , Interface (..)
  , new
  , zero
  , constant
  , xyz
  , evaluate
  , bounds
  , derivative
  )
where

import Direction3d (Direction3d)
import Float qualified
import OpenSolid
import Surface1d qualified
import Surface1d.Function qualified
import Units (Erase)
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Vector3d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: Parameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Vector3d (space @ units) ->
    Function (space @ units)
  XYZ ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Negated ::
    Function (space @ units) ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Product1d3d' ::
    Surface1d.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :/: units2))
  CrossProduct' ::
    Function (space @ units1) ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Function (space @ units1)) (Function (space_ @ units2))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance Negation (Function (space @ units)) where
  negate function = case function of
    Coerce f -> Coerce -f
    Constant v -> Constant -v
    XYZ x y z -> XYZ -x -y -z
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d3d' f1 f2 -> Product1d3d' -f1 f2
    Product3d1d' f1 f2 -> Product3d1d' f1 -f2
    _ -> Negated function

instance Multiplication Sign (Function (space @ units)) (Function (space @ units))

instance Multiplication' Sign (Function (space @ units)) where
  type Sign .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function (space @ units)) Sign (Function (space @ units))

instance Multiplication' (Function (space @ units)) Sign where
  type Function (space @ units) .*. Sign = Function (space @ (units :*: Unitless))
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  f1 + Constant v | v == Vector3d.zero = f1
  Constant v + f2 | v == Vector3d.zero = f2
  f1 + Negated f2 = f1 - f2
  Negated f1 + f2 = f2 - f1
  f1 + f2 = Sum f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f + v = f + constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v + f = constant v + f

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  f1 - Constant v | v == Vector3d.zero = f1
  Constant v - f2 | v == Vector3d.zero = -f2
  f1 - Negated f2 = f1 + f2
  f1 - f2 = Difference f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f - v = f - constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface1d.Function units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Function (space @ units2)) where
  type
    Surface1d.Function units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  Surface1d.Function.Constant (Qty 0.0) .*. _ = zero
  Surface1d.Function.Constant (Qty 1.0) .*. f2 = Units.coerce f2
  Surface1d.Function.Constant (Qty -1.0) .*. f2 = Units.coerce -f2
  _ .*. Constant v | v == Vector3d.zero = zero
  f1 .*. f2 = Product1d3d' f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Qty units1) (Function (space @ units2)) where
  type Qty units1 .*. Function (space @ units2) = Function (space @ (units1 :*: units2))
  f1 .*. f2 = Surface1d.Function.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface1d.Function units1) (Vector3d (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Vector3d (space @ units2)) where
  type
    Surface1d.Function units1 .*. Vector3d (space @ units2) =
      Function (space @ (units1 :*: units2))
  function .*. vector = function .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  Constant v .*. _ | v == Vector3d.zero = zero
  _ .*. Surface1d.Function.Constant (Qty 0.0) = zero
  f1 .*. Surface1d.Function.Constant (Qty 1.0) = Units.coerce f1
  f1 .*. Surface1d.Function.Constant (Qty -1.0) = Units.coerce -f1
  f1 .*. f2 = Product3d1d' f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Qty units2) where
  type Function (space @ units1) .*. Qty units2 = Function (space @ (units1 :*: units2))
  function .*. value = function .*. Surface1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Surface1d.Function units2) where
  type
    Vector3d (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  vector .*. function = constant vector .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) ./. Surface1d.Function units2 =
      Function (space @ (units1 :/: units2))
  Constant v ./. _ | v == Vector3d.zero = zero
  f1 ./. Surface1d.Function.Constant x = (1 ./. x) .*^ f1
  f1 ./. f2 = Quotient' f1 f2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Qty units2) where
  type
    Function (space @ units1) ./. Qty units2 =
      Function (space @ (units1 :/: units2))
  function ./. value = function ./. Surface1d.Function.constant value

instance Division' (Function (space @ units)) Int where
  type Function (space @ units) ./. Int = Function (space @ (units :/: Unitless))
  function ./. value = function ./. Float.int value

instance Division (Function (space @ units)) Int (Function (space @ units))

instance Multiplication' (Function (space @ units)) Int where
  type Function (space @ units) .*. Int = Function (space @ (units :*: Unitless))
  function .*. scale = function .*. Float.int scale

instance Multiplication' Int (Function (space @ units)) where
  type Int .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  scale .*. function = Float.int scale .*. function

instance Multiplication (Function (space @ units)) Int (Function (space @ units))

instance Multiplication Int (Function (space @ units)) (Function (space @ units))

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Function (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Function (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  Constant v .><. _ | v == Vector3d.zero = zero
  _ .><. Constant v | v == Vector3d.zero = zero
  f1 .><. f2 = CrossProduct' f1 f2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Vector3d (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Vector3d (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  f .><. v = f .><. constant v

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Vector3d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector3d (space @ units1) .><. Function (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  v .><. f = constant v .><. f

data DotProduct' space units1 units2
  = DotProduct' (Function (space @ units1)) (Function (space @ units2))

deriving instance Show (DotProduct' space units1 units2)

instance Surface1d.Function.Interface (DotProduct' space units1 units2) (units1 :*: units2) where
  evaluateImpl (DotProduct' f1 f2) t = evaluate f1 t .<>. evaluate f2 t
  boundsImpl (DotProduct' f1 f2) t = bounds f1 t .<>. bounds f2 t
  derivativeImpl parameter (DotProduct' f1 f2) =
    derivative parameter f1 .<>. f2 + f1 .<>. derivative parameter f2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units1)) (Function (space_ @ units2))
  where
  type
    Function (space @ units1) .<>. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  Constant v .<>. _ | v == Vector3d.zero = Surface1d.Function.zero
  _ .<>. Constant v | v == Vector3d.zero = Surface1d.Function.zero
  f1 .<>. f2 = Surface1d.Function.new (DotProduct' f1 f2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units1)) (Vector3d (space_ @ units2))
  where
  type
    Function (space @ units1) .<>. Vector3d (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector3d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector3d (space @ units1) .<>. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  vector .<>. function = constant vector .<>. function

instance
  space ~ space_ =>
  DotMultiplication (Function (space @ units)) (Direction3d space_) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units)) (Direction3d space_)
  where
  type Function (space @ units) .<>. Direction3d space_ = Surface1d.Function (units :*: Unitless)
  function .<>. direction = function .<>. Vector3d.unit direction

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (Function (space_ @ units)) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction3d space) (Function (space_ @ units))
  where
  type Direction3d space .<>. Function (space_ @ units) = Surface1d.Function (Unitless :*: units)
  direction .<>. function = Vector3d.unit direction .<>. function

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    VectorCurve3d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition coordinateSystem)

instance
  Composition
    (Surface1d.Function Unitless)
    (VectorCurve3d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance Interface (SurfaceCurveComposition (space @ units)) (space @ units) where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    VectorCurve3d.evaluateAt (Surface1d.Function.evaluate function uv) curve

  boundsImpl (SurfaceCurveComposition function curve) uv =
    VectorCurve3d.segmentBounds (Surface1d.Function.bounds function uv) curve

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (VectorCurve3d.derivative curve . function) * Surface1d.Function.derivative parameter function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Constant

xyz ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xyz = XYZ

evaluate :: Function (space @ units) -> Uv.Point -> Vector3d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XYZ x y z ->
    Vector3d.xyz
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
      (Surface1d.Function.evaluate z uv)
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product1d3d' f1 f2 -> Surface1d.Function.evaluate f1 uv .*. evaluate f2 uv
  Product3d1d' f1 f2 -> evaluate f1 uv .*. Surface1d.Function.evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. Surface1d.Function.evaluate f2 uv
  CrossProduct' f1 f2 -> evaluate f1 uv .><. evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> VectorBounds3d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> VectorBounds3d.constant v
  XYZ x y z ->
    VectorBounds3d.xyz
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
      (Surface1d.Function.bounds z uv)
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product1d3d' f1 f2 -> Surface1d.Function.bounds f1 uv .*. bounds f2 uv
  Product3d1d' f1 f2 -> bounds f1 uv .*. Surface1d.Function.bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. Surface1d.Function.bounds f2 uv
  CrossProduct' f1 f2 -> bounds f1 uv .><. bounds f2 uv

derivative :: Uv.Parameter -> Function (space @ units) -> Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Coerce (derivative parameter f)
  Constant _ -> zero
  XYZ x y z ->
    XYZ
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
      (Surface1d.Function.derivative parameter z)
  Negated f -> -(derivative parameter f)
  Sum f1 f2 -> derivative parameter f1 + derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - derivative parameter f2
  Product1d3d' f1 f2 ->
    Surface1d.Function.derivative parameter f1 .*. f2 + f1 .*. derivative parameter f2
  Product3d1d' f1 f2 ->
    derivative parameter f1 .*. f2 + f1 .*. Surface1d.Function.derivative parameter f2
  Quotient' f1 f2 ->
    (derivative parameter f1 .*. f2 - f1 .*. Surface1d.Function.derivative parameter f2)
      .!/.! Surface1d.Function.squared' f2
  CrossProduct' f1 f2 -> derivative parameter f1 .><. f2 + f1 .><. derivative parameter f2
