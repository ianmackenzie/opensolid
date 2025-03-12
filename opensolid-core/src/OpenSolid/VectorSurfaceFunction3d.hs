module OpenSolid.VectorSurfaceFunction3d
  ( VectorSurfaceFunction3d (Parametric)
  , Interface (..)
  , new
  , zero
  , constant
  , xyz
  , evaluate
  , evaluateBounds
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Composition
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface3d qualified as Expression.VectorSurface3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction3d coordinateSystem

data VectorSurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction3d ::
    Interface function (space @ units) =>
    function ->
    VectorSurfaceFunction3d (space @ units)
  Coerce ::
    VectorSurfaceFunction3d (space @ units1) ->
    VectorSurfaceFunction3d (space @ units2)
  Parametric ::
    Expression UvPoint (Vector3d (space @ units)) ->
    VectorSurfaceFunction3d (space @ units)
  XYZ ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units ->
    VectorSurfaceFunction3d (space @ units)
  Negated ::
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units)
  Sum ::
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units)
  Difference ::
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units)
  Product1d3d' ::
    SurfaceFunction units1 ->
    VectorSurfaceFunction3d (space @ units2) ->
    VectorSurfaceFunction3d (space @ (units1 :*: units2))
  Product3d1d' ::
    VectorSurfaceFunction3d (space @ units1) ->
    SurfaceFunction units2 ->
    VectorSurfaceFunction3d (space @ (units1 :*: units2))
  Quotient' ::
    VectorSurfaceFunction3d (space @ units1) ->
    SurfaceFunction units2 ->
    VectorSurfaceFunction3d (space @ (units1 :/: units2))
  CrossProduct' ::
    VectorSurfaceFunction3d (space @ units1) ->
    VectorSurfaceFunction3d (space @ units2) ->
    VectorSurfaceFunction3d (space @ (units1 :*: units2))
  PlaceIn ::
    Basis3d global (Defines local) ->
    VectorSurfaceFunction3d (local @ units) ->
    VectorSurfaceFunction3d (global @ units)
  Transformed ::
    Transform3d tag (space @ translationUnits) ->
    VectorSurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units)

deriving instance Show (VectorSurfaceFunction3d (space @ units))

instance
  HasUnits
    (VectorSurfaceFunction3d (space @ units))
    units
    (VectorSurfaceFunction3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d (space1 @ unitsA))
    (VectorSurfaceFunction3d (space2 @ unitsB))
  where
  coerce function = case function of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce f -> Coerce f
    _ -> Coerce function

instance Negation (VectorSurfaceFunction3d (space @ units)) where
  negate function = case function of
    Coerce f -> Coerce -f
    Parametric expression -> Parametric -expression
    XYZ x y z -> XYZ -x -y -z
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d3d' f1 f2 -> Product1d3d' -f1 f2
    Product3d1d' f1 f2 -> Product3d1d' f1 -f2
    _ -> Negated function

instance
  Multiplication
    Sign
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction3d (space @ units))
    Sign
    (VectorSurfaceFunction3d (space @ units))
  where
  function * Positive = function
  function * Negative = -function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + Negated rhs = lhs - rhs
  Negated lhs + rhs = rhs - lhs
  lhs + rhs = Sum lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorSurfaceFunction3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  f + v = f + constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  v + f = constant v + f

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - Negated rhs = lhs + rhs
  lhs - rhs = Difference lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  f - v = f - constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  v - f = constant v - f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  point + function = SurfaceFunction3d.constant point + function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  SurfaceFunction.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorSurfaceFunction3d (space @ units2)) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  f1 .*. f2 = SurfaceFunction.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorSurfaceFunction3d (space @ units1)) (SurfaceFunction units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  Parametric lhs .*. SurfaceFunction.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorSurfaceFunction3d (space @ units1)) (Qty units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  function .*. value = function .*. SurfaceFunction.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorSurfaceFunction3d (space @ units1)) (SurfaceFunction units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
  where
  Parametric lhs ./. SurfaceFunction.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorSurfaceFunction3d (space @ units1)) (Qty units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction3d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
  where
  function ./. value = function ./. SurfaceFunction.constant value

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  Parametric lhs `cross'` Parametric rhs = Parametric (lhs `cross'` rhs)
  lhs `cross'` rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  f `cross'` v = f `cross'` constant v

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  v `cross'` f = constant v `cross'` f

data DotProduct' space units1 units2
  = DotProduct' (VectorSurfaceFunction3d (space @ units1)) (VectorSurfaceFunction3d (space @ units2))

deriving instance Show (DotProduct' space units1 units2)

instance SurfaceFunction.Interface (DotProduct' space units1 units2) (units1 :*: units2) where
  evaluateImpl (DotProduct' f1 f2) tValue =
    evaluate f1 tValue `dot'` evaluate f2 tValue

  evaluateBoundsImpl (DotProduct' f1 f2) tRange =
    evaluateBounds f1 tRange `dot'` evaluateBounds f2 tRange

  derivativeImpl parameter (DotProduct' f1 f2) =
    derivative parameter f1 `dot'` f2 + f1 `dot'` derivative parameter f2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  Parametric lhs `dot'` Parametric rhs = SurfaceFunction.Parametric (lhs `dot'` rhs)
  lhs `dot'` rhs = SurfaceFunction.new (DotProduct' lhs rhs)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  function `dot'` vector = function `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  vector `dot'` function = constant vector `dot'` function

instance
  space ~ space_ =>
  DotMultiplication (VectorSurfaceFunction3d (space @ units)) (Direction3d space_) (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (VectorSurfaceFunction3d (space_ @ units)) (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  Parametric outer . SurfaceFunction2d.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  uvCoordinates ~ UvCoordinates =>
  Interface
    (VectorSurfaceFunction3d (space @ units) :.: SurfaceFunction2d uvCoordinates)
    (space @ units)
  where
  evaluateImpl (outer :.: inner) uvValue =
    evaluate outer (SurfaceFunction2d.evaluate inner uvValue)

  evaluateBoundsImpl (outer :.: inner) uvBounds =
    evaluateBounds outer (SurfaceFunction2d.evaluateBounds inner uvBounds)

  derivativeImpl varyingParameter (outer :.: inner) = do
    let duOuter = derivative U outer . inner
    let dvOuter = derivative V outer . inner
    let dInner = SurfaceFunction2d.derivative varyingParameter inner
    duOuter * VectorSurfaceFunction2d.xComponent dInner + dvOuter * VectorSurfaceFunction2d.yComponent dInner

new :: Interface function (space @ units) => function -> VectorSurfaceFunction3d (space @ units)
new = VectorSurfaceFunction3d

zero :: VectorSurfaceFunction3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorSurfaceFunction3d (space @ units)
constant = Parametric . Expression.constant

xyz ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  VectorSurfaceFunction3d (space @ units)
xyz = XYZ

evaluate :: VectorSurfaceFunction3d (space @ units) -> UvPoint -> Vector3d (space @ units)
evaluate function uvPoint = case function of
  VectorSurfaceFunction3d f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XYZ x y z ->
    Vector3d.xyz
      (SurfaceFunction.evaluate x uvPoint)
      (SurfaceFunction.evaluate y uvPoint)
      (SurfaceFunction.evaluate z uvPoint)
  Negated f -> negate (evaluate f uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - evaluate f2 uvPoint
  Product1d3d' f1 f2 -> SurfaceFunction.evaluate f1 uvPoint .*. evaluate f2 uvPoint
  Product3d1d' f1 f2 -> evaluate f1 uvPoint .*. SurfaceFunction.evaluate f2 uvPoint
  Quotient' f1 f2 -> evaluate f1 uvPoint ./. SurfaceFunction.evaluate f2 uvPoint
  CrossProduct' f1 f2 -> evaluate f1 uvPoint `cross'` evaluate f2 uvPoint
  PlaceIn frame f -> Vector3d.placeIn frame (evaluate f uvPoint)
  Transformed transform f -> Vector3d.transformBy transform (evaluate f uvPoint)

evaluateBounds :: VectorSurfaceFunction3d (space @ units) -> UvBounds -> VectorBounds3d (space @ units)
evaluateBounds function uvBounds = case function of
  VectorSurfaceFunction3d f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XYZ x y z ->
    VectorBounds3d
      (SurfaceFunction.evaluateBounds x uvBounds)
      (SurfaceFunction.evaluateBounds y uvBounds)
      (SurfaceFunction.evaluateBounds z uvBounds)
  Negated f -> negate (evaluateBounds f uvBounds)
  Sum f1 f2 -> evaluateBounds f1 uvBounds + evaluateBounds f2 uvBounds
  Difference f1 f2 -> evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds
  Product1d3d' f1 f2 -> SurfaceFunction.evaluateBounds f1 uvBounds .*. evaluateBounds f2 uvBounds
  Product3d1d' f1 f2 -> evaluateBounds f1 uvBounds .*. SurfaceFunction.evaluateBounds f2 uvBounds
  Quotient' f1 f2 -> evaluateBounds f1 uvBounds ./. SurfaceFunction.evaluateBounds f2 uvBounds
  CrossProduct' f1 f2 -> evaluateBounds f1 uvBounds `cross'` evaluateBounds f2 uvBounds
  PlaceIn frame f -> VectorBounds3d.placeIn frame (evaluateBounds f uvBounds)
  Transformed transform f -> VectorBounds3d.transformBy transform (evaluateBounds f uvBounds)

derivative :: SurfaceParameter -> VectorSurfaceFunction3d (space @ units) -> VectorSurfaceFunction3d (space @ units)
derivative parameter function = case function of
  VectorSurfaceFunction3d f -> derivativeImpl parameter f
  Coerce f -> Coerce (derivative parameter f)
  Parametric expression -> Parametric (Expression.surfaceDerivative parameter expression)
  XYZ x y z ->
    XYZ
      (SurfaceFunction.derivative parameter x)
      (SurfaceFunction.derivative parameter y)
      (SurfaceFunction.derivative parameter z)
  Negated f -> -(derivative parameter f)
  Sum f1 f2 -> derivative parameter f1 + derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - derivative parameter f2
  Product1d3d' f1 f2 ->
    SurfaceFunction.derivative parameter f1 .*. f2 + f1 .*. derivative parameter f2
  Product3d1d' f1 f2 ->
    derivative parameter f1 .*. f2 + f1 .*. SurfaceFunction.derivative parameter f2
  Quotient' f1 f2 ->
    (derivative parameter f1 .*. f2 - f1 .*. SurfaceFunction.derivative parameter f2)
      .!/.! SurfaceFunction.squared' f2
  CrossProduct' f1 f2 -> derivative parameter f1 `cross'` f2 + f1 `cross'` derivative parameter f2
  PlaceIn basis f -> placeIn basis (derivative parameter f)
  Transformed transform f -> transformBy transform (derivative parameter f)

placeIn ::
  Basis3d global (Defines local) ->
  VectorSurfaceFunction3d (local @ units) ->
  VectorSurfaceFunction3d (global @ units)
placeIn basis f = case f of
  Parametric expression -> Parametric (Expression.VectorSurface3d.placeIn basis expression)
  Coerce function -> Coerce (placeIn basis function)
  function -> PlaceIn basis function

relativeTo ::
  Basis3d global (Defines local) ->
  VectorSurfaceFunction3d (global @ units) ->
  VectorSurfaceFunction3d (local @ units)
relativeTo basis function = placeIn (Basis3d.inverse basis) function

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
transformBy transform function = case function of
  Coerce f -> Coerce (transformBy transform f)
  Parametric expression -> Parametric (Expression.VectorSurface3d.transformBy transform expression)
  Negated f -> Negated (transformBy transform f)
  Sum f1 f2 -> Sum (transformBy transform f1) (transformBy transform f2)
  Difference f1 f2 -> Difference (transformBy transform f1) (transformBy transform f2)
  Product1d3d' f1 f2 -> Product1d3d' f1 (transformBy transform f2)
  Product3d1d' f1 f2 -> Product3d1d' (transformBy transform f1) f2
  Quotient' f1 f2 -> Quotient' (transformBy transform f1) f2
  Transformed existing f -> do
    let transform1 = Units.erase (Transform3d.toAffine existing)
    let transform2 = Units.erase (Transform3d.toAffine transform)
    Transformed (transform1 >> transform2) f
  _ -> Transformed transform function
