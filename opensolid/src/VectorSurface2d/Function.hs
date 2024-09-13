-- Needed for 'Surface1d.Function * Vector2d = Function'
-- and 'Vector2d * Surface1d.Function = Function' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Surface1d.Function or Vector2d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorSurface2d.Function
  ( Function (Constant)
  , Interface (..)
  , new
  , zero
  , constant
  , xy
  , evaluate
  , bounds
  , derivative
  )
where

import Direction2d (Direction2d)
import Float qualified
import OpenSolid
import Qty qualified
import Surface1d qualified
import Surface1d.Function qualified
import Units (Erase)
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBounds2d (VectorBounds2d)
import VectorBounds2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Vector2d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> VectorBounds2d coordinateSystem
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
    Vector2d (space @ units) ->
    Function (space @ units)
  XY ::
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
  Product1d2d' ::
    Surface1d.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product2d1d' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :/: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance Negation (Function (space @ units)) where
  negate function = case function of
    Coerce f -> Coerce -f
    Constant v -> Constant -v
    XY x y -> XY -x -y
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d2d' f1 f2 -> Product1d2d' -f1 f2
    Product2d1d' f1 f2 -> Product2d1d' f1 -f2
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
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 + Constant v | v == Vector2d.zero = f1
  Constant v + f2 | v == Vector2d.zero = f2
  f1 + Negated f2 = f1 - f2
  Negated f1 + f2 = f2 - f1
  f1 + f2 = Sum f1 f2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f + v = f + constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  v + f = constant v + f

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 - Constant v | v == Vector2d.zero = f1
  Constant v - f2 | v == Vector2d.zero = -f2
  f1 - Negated f2 = f1 + f2
  f1 - f2 = Difference f1 f2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f - v = f - constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface1d.Function units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Function (space @ units2)) where
  type
    Surface1d.Function units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  Surface1d.Function.Constant x .*. _ | x == Qty.zero = zero
  Surface1d.Function.Constant x .*. f2 | x == Units.coerce 1.0 = Units.coerce f2
  Surface1d.Function.Constant x .*. f2 | x == Units.coerce -1.0 = Units.coerce -f2
  _ .*. Constant v | v == Vector2d.zero = zero
  f1 .*. f2 = Product1d2d' f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Qty units1) (Function (space @ units2)) where
  type Qty units1 .*. Function (space @ units2) = Function (space @ (units1 :*: units2))
  f1 .*. f2 = Surface1d.Function.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface1d.Function units1) (Vector2d (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Vector2d (space @ units2)) where
  type
    Surface1d.Function units1 .*. Vector2d (space @ units2) =
      Function (space @ (units1 :*: units2))
  function .*. vector = function .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  Constant v .*. _ | v == Vector2d.zero = zero
  _ .*. Surface1d.Function.Constant x | x == Qty.zero = zero
  f1 .*. Surface1d.Function.Constant x | x == Units.coerce 1.0 = Units.coerce f1
  f1 .*. Surface1d.Function.Constant x | x == Units.coerce -1.0 = Units.coerce -f1
  f1 .*. f2 = Product2d1d' f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Qty units2) where
  type Function (space @ units1) .*. Qty units2 = Function (space @ (units1 :*: units2))
  function .*. value = function .*. Surface1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Vector2d (space @ units1)) (Surface1d.Function units2) where
  type
    Vector2d (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  vector .*. function = constant vector .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) ./. Surface1d.Function units2 =
      Function (space @ (units1 :/: units2))
  Constant v ./. _ | v == Vector2d.zero = zero
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

data CrossProduct' space units1 units2
  = CrossProduct' (Function (space @ units1)) (Function (space @ units2))

deriving instance Show (CrossProduct' space units1 units2)

instance Surface1d.Function.Interface (CrossProduct' space units1 units2) (units1 :*: units2) where
  evaluateImpl (CrossProduct' f1 f2) t = evaluate f1 t .><. evaluate f2 t
  boundsImpl (CrossProduct' f1 f2) t = bounds f1 t .><. bounds f2 t
  derivativeImpl parameter (CrossProduct' f1 f2) =
    derivative parameter f1 .><. f2 + f1 .><. derivative parameter f2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Function (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  Constant v .><. _ | v == Vector2d.zero = Surface1d.Function.zero
  _ .><. Constant v | v == Vector2d.zero = Surface1d.Function.zero
  Constant v1 .><. Constant v2 = Surface1d.Function.constant (v1 .><. v2)
  f1 .><. f2 = Surface1d.Function.new (CrossProduct' f1 f2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Vector2d (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Vector2d (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Vector2d (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  function .><. vector = function .><. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector2d (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  CrossMultiplication' (Vector2d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector2d (space @ units1) .><. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  vector .><. function = constant vector .><. function

instance
  space ~ space_ =>
  CrossMultiplication (Function (space @ units)) (Direction2d space_) (Surface1d.Function units)

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units)) (Direction2d space_)
  where
  type Function (space @ units) .><. Direction2d space_ = Surface1d.Function (units :*: Unitless)
  function .><. direction = function .><. Vector2d.unit direction

instance
  space ~ space_ =>
  CrossMultiplication (Direction2d space) (Function (space_ @ units)) (Surface1d.Function units)

instance
  space ~ space_ =>
  CrossMultiplication' (Direction2d space) (Function (space_ @ units))
  where
  type Direction2d space .><. Function (space_ @ units) = Surface1d.Function (Unitless :*: units)
  direction .><. function = Vector2d.unit direction .<>. function

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
  Constant v .<>. _ | v == Vector2d.zero = Surface1d.Function.zero
  _ .<>. Constant v | v == Vector2d.zero = Surface1d.Function.zero
  Constant v1 .<>. Constant v2 = Surface1d.Function.constant (v1 .<>. v2)
  f1 .<>. f2 = Surface1d.Function.new (DotProduct' f1 f2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Vector2d (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units1)) (Vector2d (space_ @ units2))
  where
  type
    Function (space @ units1) .<>. Vector2d (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector2d (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector2d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector2d (space @ units1) .<>. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  vector .<>. function = constant vector .<>. function

instance
  space ~ space_ =>
  DotMultiplication (Function (space @ units)) (Direction2d space_) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units)) (Direction2d space_)
  where
  type Function (space @ units) .<>. Direction2d space_ = Surface1d.Function (units :*: Unitless)
  function .<>. direction = function .<>. Vector2d.unit direction

instance
  space ~ space_ =>
  DotMultiplication (Direction2d space) (Function (space_ @ units)) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction2d space) (Function (space_ @ units))
  where
  type Direction2d space .<>. Function (space_ @ units) = Surface1d.Function (Unitless :*: units)
  direction .<>. function = Vector2d.unit direction .<>. function

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    VectorCurve2d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition coordinateSystem)

instance
  Composition
    (Surface1d.Function Unitless)
    (VectorCurve2d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance Interface (SurfaceCurveComposition (space @ units)) (space @ units) where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    VectorCurve2d.evaluateAt (Surface1d.Function.evaluate function uv) curve

  boundsImpl (SurfaceCurveComposition function curve) uv =
    VectorCurve2d.segmentBounds (Surface1d.Function.bounds function uv) curve

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (VectorCurve2d.derivative curve . function) * Surface1d.Function.derivative parameter function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> Function (space @ units)
constant = Constant

xy ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xy = XY

evaluate :: Function (space @ units) -> Uv.Point -> Vector2d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XY x y ->
    Vector2d.xy
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product1d2d' f1 f2 -> Surface1d.Function.evaluate f1 uv .*. evaluate f2 uv
  Product2d1d' f1 f2 -> evaluate f1 uv .*. Surface1d.Function.evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. Surface1d.Function.evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> VectorBounds2d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> VectorBounds2d.constant v
  XY x y ->
    VectorBounds2d.xy
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product1d2d' f1 f2 -> Surface1d.Function.bounds f1 uv .*. bounds f2 uv
  Product2d1d' f1 f2 -> bounds f1 uv .*. Surface1d.Function.bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. Surface1d.Function.bounds f2 uv

derivative :: Uv.Parameter -> Function (space @ units) -> Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Coerce (derivative parameter f)
  Constant _ -> zero
  XY x y ->
    XY
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
  Negated f -> -(derivative parameter f)
  Sum f1 f2 -> derivative parameter f1 + derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - derivative parameter f2
  Product1d2d' f1 f2 ->
    Surface1d.Function.derivative parameter f1 .*. f2 + f1 .*. derivative parameter f2
  Product2d1d' f1 f2 ->
    derivative parameter f1 .*. f2 + f1 .*. Surface1d.Function.derivative parameter f2
  Quotient' f1 f2 ->
    (derivative parameter f1 .*. f2 - f1 .*. Surface1d.Function.derivative parameter f2)
      .!/.! Surface1d.Function.squared' f2
