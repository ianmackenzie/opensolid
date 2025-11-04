module OpenSolid.VectorSurfaceFunction2d
  ( VectorSurfaceFunction2d
  , Compiled
  , new
  , recursive
  , desingularize
  , desingularized
  , zero
  , constant
  , xy
  , evaluate
  , evaluateBounds
  , xComponent
  , yComponent
  , components
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  , quotient
  , quotient#
  , unsafeQuotient
  , unsafeQuotient#
  , squaredMagnitude#
  , squaredMagnitude
  , magnitude
  , IsZero (IsZero)
  , direction
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Direction2d (Direction2d)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2d (DirectionSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2d qualified as DirectionSurfaceFunction2d
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface2d qualified as Expression.VectorSurface2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Blending qualified as SurfaceFunction.Blending
import OpenSolid.SurfaceFunction.Quotient qualified as SurfaceFunction.Quotient
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data VectorSurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction2d ::
    Compiled (space @ units) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    ~(VectorSurfaceFunction2d (space @ units)) ->
    VectorSurfaceFunction2d (space @ units)

instance
  HasField
    "xComponent"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units)
  where
  getField = xComponent

instance
  HasField
    "yComponent"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units)
  where
  getField = yComponent

instance
  HasField
    "components"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units, SurfaceFunction units)
  where
  getField = components

instance
  HasField
    "du"
    (VectorSurfaceFunction2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  getField (VectorSurfaceFunction2d _ du _) = du

instance
  HasField
    "dv"
    (VectorSurfaceFunction2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  getField (VectorSurfaceFunction2d _ _ dv) = dv

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    UvPoint
    (Vector2d coordinateSystem)
    UvBounds
    (VectorBounds2d coordinateSystem)

instance HasUnits (VectorSurfaceFunction2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d (space1 @ unitsA))
    (VectorSurfaceFunction2d (space2 @ unitsB))
  where
  coerce (VectorSurfaceFunction2d c du dv) =
    VectorSurfaceFunction2d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance Negation (VectorSurfaceFunction2d (space @ units)) where
  negate function = new (negate function.compiled) (\p -> negate (derivative p function))

instance
  Multiplication
    Sign
    (VectorSurfaceFunction2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction2d (space @ units))
    Sign
    (VectorSurfaceFunction2d (space @ units))
  where
  function * Positive = function
  function * Negative = -function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (\p -> derivative p lhs + derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  f + v = f + constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  v + f = constant v + f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (\p -> derivative p lhs - derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  f - v = f - constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication#
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))
  where
  lhs *# rhs =
    new
      @ lhs.compiled *# rhs.compiled
      @ \p -> SurfaceFunction.derivative p lhs *# rhs + lhs *# derivative p rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (Quantity units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication#
    (Quantity units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))
  where
  f1 *# f2 = SurfaceFunction.constant f1 *# f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication#
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))
  where
  lhs *# rhs =
    new
      @ lhs.compiled *# rhs.compiled
      @ \p -> derivative p lhs *# rhs + lhs *# SurfaceFunction.derivative p rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (Quantity units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication#
    (VectorSurfaceFunction2d (space @ units1))
    (Quantity units2)
    (VectorSurfaceFunction2d (space @ (units1 *# units2)))
  where
  function *# value = function *# SurfaceFunction.constant value

instance
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
  Division
    (VectorSurfaceFunction2d (space @ units1))
    (Quantity units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs /# rhs)

instance
  Division#
    (VectorSurfaceFunction2d (space @ units1))
    (Quantity units2)
    (VectorSurfaceFunction2d (space @ (units1 /# units2)))
  where
  function /# value = Units.simplify (function *# (1.0 /# value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross#` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  lhs `cross#` rhs =
    SurfaceFunction.new
      (lhs.compiled `cross#` rhs.compiled)
      (\p -> derivative p lhs `cross#` rhs + lhs `cross#` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross#` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  function `cross#` vector = function `cross#` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross#` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  vector `cross#` function = constant vector `cross#` function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units))
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `cross` rhs = lhs `cross` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d (space2 @ units))
    (SurfaceFunction units)
  where
  lhs `cross` rhs = Vector2d.unit lhs `cross` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot#` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  lhs `dot#` rhs =
    SurfaceFunction.new
      (lhs.compiled `dot#` rhs.compiled)
      (\p -> derivative p lhs `dot#` rhs + lhs `dot#` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot#` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  function `dot#` vector = function `dot#` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot#` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication#
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 *# units2))
  where
  vector `dot#` function = constant vector `dot#` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units))
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d (space2 @ units))
    (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector2d.unit lhs `dot` rhs

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (VectorSurfaceFunction2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  function . curve = do
    let (dudt, dvdt) = curve.derivative.components
    VectorCurve2d.new
      @ function.compiled . curve.compiled
      @ (function.du . curve) * dudt + (function.dv . curve) * dvdt

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2d (space @ units))
    (Compiled (space @ units))
  where
  getField (VectorSurfaceFunction2d c _ _) = c

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  VectorSurfaceFunction2d (space @ units)
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction2d dv.compiled du.dv dv.dv
  VectorSurfaceFunction2d c du dv'

recursive ::
  Compiled (space @ units) ->
  ( VectorSurfaceFunction2d (space @ units) ->
    SurfaceParameter ->
    VectorSurfaceFunction2d (space @ units)
  ) ->
  VectorSurfaceFunction2d (space @ units)
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  VectorSurfaceFunction2d (space @ units) ->
  "singularityU0"
    ::: Maybe (VectorSurfaceFunction2d (space @ units), VectorSurfaceFunction2d (space @ units)) ->
  "singularityU1"
    ::: Maybe (VectorSurfaceFunction2d (space @ units), VectorSurfaceFunction2d (space @ units)) ->
  "singularityV0"
    ::: Maybe (VectorSurfaceFunction2d (space @ units), VectorSurfaceFunction2d (space @ units)) ->
  "singularityV1"
    ::: Maybe (VectorSurfaceFunction2d (space @ units), VectorSurfaceFunction2d (space @ units)) ->
  VectorSurfaceFunction2d (space @ units)
desingularize = SurfaceFunction.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction Unitless ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

zero :: VectorSurfaceFunction2d (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> VectorSurfaceFunction2d (space @ units)
constant value = new (CompiledFunction.constant value) (const zero)

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  VectorSurfaceFunction2d (space @ units)
xy x y =
  new
    @ CompiledFunction.map2
      Expression.xy
      Vector2d
      VectorBounds2d
      x.compiled
      y.compiled
    @ \p -> xy (SurfaceFunction.derivative p x) (SurfaceFunction.derivative p y)

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  VectorSurfaceFunction2d (local @ units) ->
  VectorSurfaceFunction2d (global @ units)
placeIn frame function =
  new
    @ CompiledFunction.map
      (Expression.VectorSurface2d.placeIn frame)
      (Vector2d.placeIn frame)
      (VectorBounds2d.placeIn frame)
      function.compiled
    @ \p -> placeIn frame (derivative p function)

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  VectorSurfaceFunction2d (global @ units) ->
  VectorSurfaceFunction2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
transformBy transform function =
  new
    @ CompiledFunction.map
      (Expression.VectorSurface2d.transformBy transform)
      (Vector2d.transformBy transform)
      (VectorBounds2d.transformBy transform)
      function.compiled
    @ \p -> transformBy transform (derivative p function)

evaluate :: VectorSurfaceFunction2d (space @ units) -> UvPoint -> Vector2d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction2d (space @ units) ->
  UvBounds ->
  VectorBounds2d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
derivative U = (.du)
derivative V = (.dv)

xComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
xComponent function =
  SurfaceFunction.new
    @ CompiledFunction.map
      Expression.xComponent
      Vector2d.xComponent
      VectorBounds2d.xComponent
      function.compiled
    @ \parameter -> (derivative parameter function).xComponent

yComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
yComponent function =
  SurfaceFunction.new
    @ CompiledFunction.map
      Expression.yComponent
      Vector2d.yComponent
      VectorBounds2d.yComponent
      function.compiled
    @ \parameter -> (derivative parameter function).yComponent

components ::
  VectorSurfaceFunction2d (space @ units) ->
  (SurfaceFunction units, SurfaceFunction units)
components function = (xComponent function, yComponent function)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction2d (space @ units3))
quotient lhs rhs = Units.specialize (quotient# lhs rhs)

quotient# ::
  Tolerance units2 =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction2d (space @ (units1 /# units2)))
quotient# numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = SurfaceFunction.derivative p denominator
        let denominator'' = SurfaceFunction.derivative p denominator'
        let value = unsafeQuotient# numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient#
                  (numerator'' *# denominator' - numerator' *# denominator'')
                  (2.0 * SurfaceFunction.squared# denominator')
        (value, firstDerivative)
  SurfaceFunction.Quotient.impl unsafeQuotient# lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d (space @ units3)
unsafeQuotient lhs rhs = Units.specialize (unsafeQuotient# lhs rhs)

unsafeQuotient# ::
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d (space @ (units1 /# units2))
unsafeQuotient# lhs rhs =
  recursive
    @ CompiledFunction.map2 (/#) (/#) (/#) lhs.compiled rhs.compiled
    @ \self p ->
      unsafeQuotient# (derivative p lhs) rhs
        - self * SurfaceFunction.unsafeQuotient (SurfaceFunction.derivative p rhs) rhs

squaredMagnitude# :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction (units *# units)
squaredMagnitude# function =
  SurfaceFunction.new
    @ CompiledFunction.map
      Expression.squaredMagnitude#
      Vector2d.squaredMagnitude#
      VectorBounds2d.squaredMagnitude#
      function.compiled
    @ \p -> 2.0 * function `dot#` derivative p function

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2
squaredMagnitude = Units.specialize . squaredMagnitude#

magnitude :: Tolerance units => VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
magnitude function = SurfaceFunction.sqrt# (squaredMagnitude# function)

data IsZero = IsZero deriving (Eq, Show, Error.Message)

direction ::
  Tolerance units =>
  VectorSurfaceFunction2d (space @ units) ->
  Result IsZero (DirectionSurfaceFunction2d space)
direction function = case quotient function (magnitude function) of
  Failure DivisionByZero -> Failure IsZero
  Success normalizedFunction -> Success (DirectionSurfaceFunction2d.unsafe normalizedFunction)
