module OpenSolid.VectorSurfaceFunction3d
  ( VectorSurfaceFunction3d
  , Compiled
  , new
  , recursive
  , desingularize
  , desingularized
  , zero
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  , quotient
  , quotient'
  , unsafeQuotient
  , unsafeQuotient'
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface3d qualified as Expression.VectorSurface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Blending qualified as SurfaceFunction.Blending
import OpenSolid.SurfaceFunction.Quotient qualified as SurfaceFunction.Quotient
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

data VectorSurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction3d ::
    Compiled (space @ units) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    VectorSurfaceFunction3d (space @ units)

instance
  HasField
    "du"
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  getField (VectorSurfaceFunction3d _ du _) = du

instance
  HasField
    "dv"
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  getField (VectorSurfaceFunction3d _ _ dv) = dv

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    UvPoint
    (Vector3d coordinateSystem)
    UvBounds
    (VectorBounds3d coordinateSystem)

instance HasUnits (VectorSurfaceFunction3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d (space1 @ unitsA))
    (VectorSurfaceFunction3d (space2 @ unitsB))
  where
  coerce (VectorSurfaceFunction3d c du dv) =
    VectorSurfaceFunction3d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance Negation (VectorSurfaceFunction3d (space @ units)) where
  negate function = new (negate function.compiled) (\p -> negate (derivative p function))

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
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (\p -> derivative p lhs + derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  f + v = f + constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  v + f = constant v + f

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (\p -> derivative p lhs - derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  f - v = f - constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
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
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  point - function = SurfaceFunction3d.constant point - function

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
  lhs .*. rhs =
    new
      @ lhs.compiled .*. rhs.compiled
      @ \p -> SurfaceFunction.derivative p lhs .*. rhs + lhs .*. derivative p rhs

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
  lhs .*. rhs =
    new
      @ lhs.compiled .*. rhs.compiled
      @ \p -> derivative p lhs .*. rhs + lhs .*. SurfaceFunction.derivative p rhs

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
  Division (VectorSurfaceFunction3d (space @ units1)) (Qty units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction3d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
  where
  function ./. value = Units.simplify (function .*. (1.0 ./. value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ (units1 :*: units2)))
  where
  lhs `cross'` rhs =
    new
      (lhs.compiled `cross'` rhs.compiled)
      (\p -> derivative p lhs `cross'` rhs + lhs `cross'` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ (units1 :*: units2)))
  where
  f `cross'` v = f `cross'` constant v

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ (units1 :*: units2)))
  where
  v `cross'` f = constant v `cross'` f

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  lhs `dot'` rhs =
    SurfaceFunction.new
      (lhs.compiled `dot'` rhs.compiled)
      (\p -> derivative p lhs `dot'` rhs + lhs `dot'` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  function `dot'` vector = function `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  vector `dot'` function = constant vector `dot'` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction3d (space1 @ units))
    (Direction3d space2)
    (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction3d space1)
    (VectorSurfaceFunction3d (space2 @ units))
    (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3d (space @ units))
    (Compiled (space @ units))
  where
  getField (VectorSurfaceFunction3d c _ _) = c

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction3d (space @ units)) ->
  VectorSurfaceFunction3d (space @ units)
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3d dv.compiled du.dv dv.dv
  VectorSurfaceFunction3d c du dv'

recursive ::
  Compiled (space @ units) ->
  ( VectorSurfaceFunction3d (space @ units) ->
    SurfaceParameter ->
    VectorSurfaceFunction3d (space @ units)
  ) ->
  VectorSurfaceFunction3d (space @ units)
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  ( "function" ::: VectorSurfaceFunction3d (space @ units)
  , "singularityU0" ::: Maybe (VectorSurfaceFunction3d (space @ units), VectorSurfaceFunction3d (space @ units))
  , "singularityU1" ::: Maybe (VectorSurfaceFunction3d (space @ units), VectorSurfaceFunction3d (space @ units))
  , "singularityV0" ::: Maybe (VectorSurfaceFunction3d (space @ units), VectorSurfaceFunction3d (space @ units))
  , "singularityV1" ::: Maybe (VectorSurfaceFunction3d (space @ units), VectorSurfaceFunction3d (space @ units))
  ) ->
  VectorSurfaceFunction3d (space @ units)
desingularize = SurfaceFunction.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction Unitless ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

zero :: VectorSurfaceFunction3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorSurfaceFunction3d (space @ units)
constant value = new (CompiledFunction.constant value) (always zero)

evaluate :: VectorSurfaceFunction3d (space @ units) -> UvPoint -> Vector3d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction3d (space @ units) ->
  UvBounds ->
  VectorBounds3d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
derivative U = (.du)
derivative V = (.dv)

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorSurfaceFunction3d (local @ units) ->
  VectorSurfaceFunction3d (global @ units)
placeIn frame function =
  new
    @ CompiledFunction.map
      (Expression.VectorSurface3d.placeIn frame)
      (Vector3d.placeIn frame)
      (VectorBounds3d.placeIn frame)
      function.compiled
    @ \p -> placeIn frame (derivative p function)

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorSurfaceFunction3d (global @ units) ->
  VectorSurfaceFunction3d (local @ units)
relativeTo frame function = placeIn (Frame3d.inverse frame) function

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
transformBy transform function =
  new
    @ CompiledFunction.map
      (Expression.VectorSurface3d.transformBy transform)
      (Vector3d.transformBy transform)
      (VectorBounds3d.transformBy transform)
      function.compiled
    @ \p -> transformBy transform (derivative p function)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction3d (space @ units1) ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction3d (space @ units3))
quotient lhs rhs = Units.specialize (quotient' lhs rhs)

quotient' ::
  Tolerance units2 =>
  VectorSurfaceFunction3d (space @ units1) ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
quotient' numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = SurfaceFunction.derivative p denominator
        let denominator'' = SurfaceFunction.derivative p denominator'
        let value = unsafeQuotient' numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient'
                  (numerator'' .*. denominator' - numerator' .*. denominator'')
                  (2.0 * SurfaceFunction.squared' denominator')
        (value, firstDerivative)
  SurfaceFunction.Quotient.impl unsafeQuotient' lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorSurfaceFunction3d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction3d (space @ units3)
unsafeQuotient lhs rhs = Units.specialize (unsafeQuotient' lhs rhs)

unsafeQuotient' ::
  VectorSurfaceFunction3d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction3d (space @ (units1 :/: units2))
unsafeQuotient' lhs rhs =
  recursive
    @ CompiledFunction.map2 (./.) (./.) (./.) lhs.compiled rhs.compiled
    @ \self p ->
      unsafeQuotient' (derivative p lhs) rhs
        - self * SurfaceFunction.unsafeQuotient (SurfaceFunction.derivative p rhs) rhs

squaredMagnitude' :: VectorSurfaceFunction3d (space @ units) -> SurfaceFunction (units :*: units)
squaredMagnitude' function =
  SurfaceFunction.new
    @ CompiledFunction.map
      Expression.squaredMagnitude'
      Vector3d.squaredMagnitude'
      VectorBounds3d.squaredMagnitude'
      function.compiled
    @ \p -> 2.0 * function `dot'` derivative p function

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction3d (space @ units1) ->
  SurfaceFunction units2
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Tolerance units => VectorSurfaceFunction3d (space @ units) -> SurfaceFunction units
magnitude function = SurfaceFunction.sqrt' (squaredMagnitude' function)
