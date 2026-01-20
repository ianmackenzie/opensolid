module OpenSolid.VectorSurfaceFunction2D
  ( VectorSurfaceFunction2D
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
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , squaredMagnitude_
  , squaredMagnitude
  , magnitude
  , IsZero (IsZero)
  , direction
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D (DirectionSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D qualified as DirectionSurfaceFunction2D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Blending qualified as SurfaceFunction1D.Blending
import OpenSolid.SurfaceFunction1D.Quotient qualified as SurfaceFunction1D.Quotient
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

data VectorSurfaceFunction2D units space
  = VectorSurfaceFunction2D
      (Compiled units space)
      ~(VectorSurfaceFunction2D units space)
      ~(VectorSurfaceFunction2D units space)

instance
  HasField
    "xComponent"
    (VectorSurfaceFunction2D units space)
    (SurfaceFunction1D units)
  where
  getField = xComponent

instance
  HasField
    "yComponent"
    (VectorSurfaceFunction2D units space)
    (SurfaceFunction1D units)
  where
  getField = yComponent

instance
  HasField
    "components"
    (VectorSurfaceFunction2D units space)
    (SurfaceFunction1D units, SurfaceFunction1D units)
  where
  getField = components

instance
  HasField
    "du"
    (VectorSurfaceFunction2D units space)
    (VectorSurfaceFunction2D units space)
  where
  getField (VectorSurfaceFunction2D _ du _) = du

instance
  HasField
    "dv"
    (VectorSurfaceFunction2D units space)
    (VectorSurfaceFunction2D units space)
  where
  getField (VectorSurfaceFunction2D _ _ dv) = dv

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector2D units space)
    UvBounds
    (VectorBounds2D units space)

instance HasUnits (VectorSurfaceFunction2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2D unitsA space1)
    (VectorSurfaceFunction2D unitsB space2)
  where
  coerce (VectorSurfaceFunction2D c du dv) =
    VectorSurfaceFunction2D (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance Negation (VectorSurfaceFunction2D units space) where
  negative function = new (negative function.compiled) (\p -> negative (derivative p function))

instance
  Multiplication
    Sign
    (VectorSurfaceFunction2D units space)
    (VectorSurfaceFunction2D units space)
  where
  Positive .*. function = function
  Negative .*. function = negative function

instance
  Multiplication
    (VectorSurfaceFunction2D units space)
    Sign
    (VectorSurfaceFunction2D units space)
  where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (\p -> derivative p lhs .+. derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  f .+. v = f .+. constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  v .+. f = constant v .+. f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (\p -> derivative p lhs .-. derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  f .-. v = f .-. constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  v .-. f = constant v .-. f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> SurfaceFunction1D.derivative p lhs ?*? rhs .+. lhs ?*? derivative p rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (Quantity units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorSurfaceFunction2D units2 space)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  f1 ?*? f2 = SurfaceFunction1D.constant f1 ?*? f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> derivative p lhs ?*? rhs .+. lhs ?*? SurfaceFunction1D.derivative p rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (VectorSurfaceFunction2D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  function ?*? value = function ?*? SurfaceFunction1D.constant value

instance
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
  Division
    (VectorSurfaceFunction2D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorSurfaceFunction2D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2D (units1 ?/? units2) space)
  where
  function ?/? value = Units.simplify (function ?*? (1 /? value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    SurfaceFunction1D.new
      (lhs.compiled `cross_` rhs.compiled)
      (\p -> derivative p lhs `cross_` rhs .+. lhs `cross_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function `cross_` vector = function `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  vector `cross_` function = constant vector `cross_` function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2D units space1)
    (Direction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `cross` rhs = lhs `cross` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2D space1)
    (VectorSurfaceFunction2D units space2)
    (SurfaceFunction1D units)
  where
  lhs `cross` rhs = Vector2D.unit lhs `cross` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    SurfaceFunction1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (\p -> derivative p lhs `dot_` rhs .+. lhs `dot_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function `dot_` vector = function `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  vector `dot_` function = constant vector `dot_` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2D units space1)
    (Direction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2D space1)
    (VectorSurfaceFunction2D units space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = Vector2D.unit lhs `dot` rhs

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2D unitless uvSpace)
    (VectorSurfaceFunction2D units space)
    (VectorCurve2D units space)
  where
  function `compose` curve = do
    let (dudt, dvdt) = VectorCurve2D.components (Curve2D.derivative curve)
    VectorCurve2D.new
      (function.compiled `compose` Curve2D.compiled curve)
      ((function.du `compose` curve) .*. dudt .+. (function.dv `compose` curve) .*. dvdt)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2D units space)
    (Compiled units space)
  where
  getField (VectorSurfaceFunction2D c _ _) = c

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2D units space) ->
  VectorSurfaceFunction2D units space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction2D dv.compiled du.dv dv.dv
  VectorSurfaceFunction2D c du dv'

recursive ::
  Compiled units space ->
  ( VectorSurfaceFunction2D units space ->
    SurfaceParameter ->
    VectorSurfaceFunction2D units space
  ) ->
  VectorSurfaceFunction2D units space
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  VectorSurfaceFunction2D units space ->
  "singularityU0"
    ::: Maybe (VectorSurfaceFunction2D units space, VectorSurfaceFunction2D units space) ->
  "singularityU1"
    ::: Maybe (VectorSurfaceFunction2D units space, VectorSurfaceFunction2D units space) ->
  "singularityV0"
    ::: Maybe (VectorSurfaceFunction2D units space, VectorSurfaceFunction2D units space) ->
  "singularityV1"
    ::: Maybe (VectorSurfaceFunction2D units space, VectorSurfaceFunction2D units space) ->
  VectorSurfaceFunction2D units space
desingularize = SurfaceFunction1D.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction1D Unitless ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

zero :: VectorSurfaceFunction2D units space
zero = constant Vector2D.zero

constant :: Vector2D units space -> VectorSurfaceFunction2D units space
constant value = new (CompiledFunction.constant value) (const zero)

xy ::
  SurfaceFunction1D units ->
  SurfaceFunction1D units ->
  VectorSurfaceFunction2D units space
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Vector2D
          VectorBounds2D
          x.compiled
          y.compiled
  new compiledXY (\p -> xy (SurfaceFunction1D.derivative p x) (SurfaceFunction1D.derivative p y))

placeIn ::
  Frame2D frameUnits global local ->
  VectorSurfaceFunction2D units local ->
  VectorSurfaceFunction2D units global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector2D.placeIn frame)
          (VectorBounds2D.placeIn frame)
          function.compiled
  new compiledPlaced (\p -> placeIn frame (derivative p function))

relativeTo ::
  Frame2D frameUnits global local ->
  VectorSurfaceFunction2D units global ->
  VectorSurfaceFunction2D units local
relativeTo frame = placeIn (Frame2D.inverse frame)

transformBy ::
  Transform2D tag translationUnits space ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector2D.transformBy transform)
          (VectorBounds2D.transformBy transform)
          function.compiled
  new compiledTransformed (\p -> transformBy transform (derivative p function))

evaluate :: VectorSurfaceFunction2D units space -> UvPoint -> Vector2D units space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction2D units space ->
  UvBounds ->
  VectorBounds2D units space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
derivative U = (.du)
derivative V = (.dv)

xComponent :: VectorSurfaceFunction2D units space -> SurfaceFunction1D units
xComponent function = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2D.xComponent
          VectorBounds2D.xComponent
          function.compiled
  SurfaceFunction1D.new
    compiledXComponent
    (\parameter -> xComponent (derivative parameter function))

yComponent :: VectorSurfaceFunction2D units space -> SurfaceFunction1D units
yComponent function = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2D.yComponent
          VectorBounds2D.yComponent
          function.compiled
  SurfaceFunction1D.new
    compiledYComponent
    (\parameter -> (derivative parameter function).yComponent)

components ::
  VectorSurfaceFunction2D units space ->
  (SurfaceFunction1D units, SurfaceFunction1D units)
components function = (xComponent function, yComponent function)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction2D units1 space ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (VectorSurfaceFunction2D units3 space)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorSurfaceFunction2D units1 space ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (VectorSurfaceFunction2D (units1 ?/? units2) space)
quotient_ numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = SurfaceFunction1D.derivative p denominator
        let denominator'' = SurfaceFunction1D.derivative p denominator'
        let value = unsafeQuotient_ numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient_
                  (numerator'' ?*? denominator' .-. numerator' ?*? denominator'')
                  (2 *. SurfaceFunction1D.squared_ denominator')
        (value, firstDerivative)
  SurfaceFunction1D.Quotient.impl unsafeQuotient_ lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorSurfaceFunction2D units1 space ->
  SurfaceFunction1D units2 ->
  VectorSurfaceFunction2D units3 space
unsafeQuotient lhs rhs = Units.specialize (unsafeQuotient_ lhs rhs)

unsafeQuotient_ ::
  VectorSurfaceFunction2D units1 space ->
  SurfaceFunction1D units2 ->
  VectorSurfaceFunction2D (units1 ?/? units2) space
unsafeQuotient_ lhs rhs = do
  let quotientDerivative self p =
        unsafeQuotient_ (derivative p lhs) rhs
          .-. self .*. SurfaceFunction1D.unsafeQuotient (SurfaceFunction1D.derivative p rhs) rhs
  recursive
    (CompiledFunction.map2 (?/?) (?/?) (?/?) lhs.compiled rhs.compiled)
    quotientDerivative

squaredMagnitude_ :: VectorSurfaceFunction2D units space -> SurfaceFunction1D (units ?*? units)
squaredMagnitude_ function = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector2D.squaredMagnitude_
          VectorBounds2D.squaredMagnitude_
          function.compiled
  SurfaceFunction1D.new
    compiledSquaredMagnitude
    (\p -> 2 *. function `dot_` derivative p function)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction2D units1 space ->
  SurfaceFunction1D units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: Tolerance units => VectorSurfaceFunction2D units space -> SurfaceFunction1D units
magnitude function = SurfaceFunction1D.sqrt_ (squaredMagnitude_ function)

data IsZero = IsZero deriving (Eq, Show)

direction ::
  Tolerance units =>
  VectorSurfaceFunction2D units space ->
  Result IsZero (DirectionSurfaceFunction2D space)
direction function = case quotient function (magnitude function) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedFunction -> Ok (DirectionSurfaceFunction2D.unsafe normalizedFunction)
