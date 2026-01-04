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
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2d (DirectionSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2d qualified as DirectionSurfaceFunction2d
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface2d qualified as Expression.VectorSurface2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Polymorphic.Vector2d (Vector2d (Vector2d))
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Blending qualified as SurfaceFunction.Blending
import OpenSolid.SurfaceFunction.Quotient qualified as SurfaceFunction.Quotient
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data VectorSurfaceFunction2d units space
  = VectorSurfaceFunction2d
      (Compiled units space)
      ~(VectorSurfaceFunction2d units space)
      ~(VectorSurfaceFunction2d units space)

instance
  HasField
    "xComponent"
    (VectorSurfaceFunction2d units space)
    (SurfaceFunction units)
  where
  getField = xComponent

instance
  HasField
    "yComponent"
    (VectorSurfaceFunction2d units space)
    (SurfaceFunction units)
  where
  getField = yComponent

instance
  HasField
    "components"
    (VectorSurfaceFunction2d units space)
    (SurfaceFunction units, SurfaceFunction units)
  where
  getField = components

instance
  HasField
    "du"
    (VectorSurfaceFunction2d units space)
    (VectorSurfaceFunction2d units space)
  where
  getField (VectorSurfaceFunction2d _ du _) = du

instance
  HasField
    "dv"
    (VectorSurfaceFunction2d units space)
    (VectorSurfaceFunction2d units space)
  where
  getField (VectorSurfaceFunction2d _ _ dv) = dv

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector2d units space)
    UvBounds
    (VectorBounds2d units space)

instance HasUnits (VectorSurfaceFunction2d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d unitsA space1)
    (VectorSurfaceFunction2d unitsB space2)
  where
  coerce (VectorSurfaceFunction2d c du dv) =
    VectorSurfaceFunction2d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance Negation (VectorSurfaceFunction2d units space) where
  negative function = new (negative function.compiled) (\p -> negative (derivative p function))

instance
  Multiplication
    Sign
    (VectorSurfaceFunction2d units space)
    (VectorSurfaceFunction2d units space)
  where
  Positive .*. function = function
  Negative .*. function = negative function

instance
  Multiplication
    (VectorSurfaceFunction2d units space)
    Sign
    (VectorSurfaceFunction2d units space)
  where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (\p -> derivative p lhs .+. derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  f .+. v = f .+. constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  v .+. f = constant v .+. f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (\p -> derivative p lhs .-. derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  f .-. v = f .-. constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (VectorSurfaceFunction2d units1 space1)
  where
  v .-. f = constant v .-. f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> SurfaceFunction.derivative p lhs ?*? rhs .+. lhs ?*? derivative p rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (Quantity units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorSurfaceFunction2d units2 space)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)
  where
  f1 ?*? f2 = SurfaceFunction.constant f1 ?*? f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2d units1 space)
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> derivative p lhs ?*? rhs .+. lhs ?*? SurfaceFunction.derivative p rhs)

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (VectorSurfaceFunction2d units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2d units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2d (units1 ?*? units2) space)
  where
  function ?*? value = function ?*? SurfaceFunction.constant value

instance
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
  Division
    (VectorSurfaceFunction2d units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2d units3 space)
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorSurfaceFunction2d units1 space)
    (Quantity units2)
    (VectorSurfaceFunction2d (units1 ?/? units2) space)
  where
  function ?/? value = Units.simplify (function ?*? (1 /? value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    SurfaceFunction.new
      (lhs.compiled `cross_` rhs.compiled)
      (\p -> derivative p lhs `cross_` rhs .+. lhs `cross_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  function `cross_` vector = function `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  vector `cross_` function = constant vector `cross_` function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2d units space1)
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `cross` rhs = lhs `cross` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d units space2)
    (SurfaceFunction units)
  where
  lhs `cross` rhs = Vector2d.unit lhs `cross` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    SurfaceFunction.new
      (lhs.compiled `dot_` rhs.compiled)
      (\p -> derivative p lhs `dot_` rhs .+. lhs `dot_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction2d units1 space1)
    (Vector2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  function `dot_` vector = function `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2d units1 space1)
    (VectorSurfaceFunction2d units2 space2)
    (SurfaceFunction (units1 ?*? units2))
  where
  vector `dot_` function = constant vector `dot_` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2d units space1)
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d units space2)
    (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector2d.unit lhs `dot` rhs

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (VectorSurfaceFunction2d units space)
    (VectorCurve2d units space)
  where
  function `compose` curve = do
    let (dudt, dvdt) = (Curve2d.derivative curve).components
    VectorCurve2d.new
      (function.compiled `compose` Curve2d.compiled curve)
      ((function.du `compose` curve) .*. dudt .+. (function.dv `compose` curve) .*. dvdt)

instance
  HasField
    "compiled"
    (VectorSurfaceFunction2d units space)
    (Compiled units space)
  where
  getField (VectorSurfaceFunction2d c _ _) = c

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2d units space) ->
  VectorSurfaceFunction2d units space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction2d dv.compiled du.dv dv.dv
  VectorSurfaceFunction2d c du dv'

recursive ::
  Compiled units space ->
  ( VectorSurfaceFunction2d units space ->
    SurfaceParameter ->
    VectorSurfaceFunction2d units space
  ) ->
  VectorSurfaceFunction2d units space
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  VectorSurfaceFunction2d units space ->
  "singularityU0"
    ::: Maybe (VectorSurfaceFunction2d units space, VectorSurfaceFunction2d units space) ->
  "singularityU1"
    ::: Maybe (VectorSurfaceFunction2d units space, VectorSurfaceFunction2d units space) ->
  "singularityV0"
    ::: Maybe (VectorSurfaceFunction2d units space, VectorSurfaceFunction2d units space) ->
  "singularityV1"
    ::: Maybe (VectorSurfaceFunction2d units space, VectorSurfaceFunction2d units space) ->
  VectorSurfaceFunction2d units space
desingularize = SurfaceFunction.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction Unitless ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

zero :: VectorSurfaceFunction2d units space
zero = constant Vector2d.zero

constant :: Vector2d units space -> VectorSurfaceFunction2d units space
constant value = new (CompiledFunction.constant value) (const zero)

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  VectorSurfaceFunction2d units space
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Vector2d
          VectorBounds2d
          x.compiled
          y.compiled
  new compiledXY (\p -> xy (SurfaceFunction.derivative p x) (SurfaceFunction.derivative p y))

placeIn ::
  Frame2d frameUnits global local ->
  VectorSurfaceFunction2d units local ->
  VectorSurfaceFunction2d units global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorSurface2d.placeIn frame)
          (Vector2d.placeIn frame)
          (VectorBounds2d.placeIn frame)
          function.compiled
  new compiledPlaced (\p -> placeIn frame (derivative p function))

relativeTo ::
  Frame2d frameUnits global local ->
  VectorSurfaceFunction2d units global ->
  VectorSurfaceFunction2d units local
relativeTo frame = placeIn (Frame2d.inverse frame)

transformBy ::
  Transform2d tag translationUnits space ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorSurface2d.transformBy transform)
          (Vector2d.transformBy transform)
          (VectorBounds2d.transformBy transform)
          function.compiled
  new compiledTransformed (\p -> transformBy transform (derivative p function))

evaluate :: VectorSurfaceFunction2d units space -> UvPoint -> Vector2d units space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction2d units space ->
  UvBounds ->
  VectorBounds2d units space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d units space ->
  VectorSurfaceFunction2d units space
derivative U = (.du)
derivative V = (.dv)

xComponent :: VectorSurfaceFunction2d units space -> SurfaceFunction units
xComponent function = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2d.xComponent
          VectorBounds2d.xComponent
          function.compiled
  SurfaceFunction.new
    compiledXComponent
    (\parameter -> xComponent (derivative parameter function))

yComponent :: VectorSurfaceFunction2d units space -> SurfaceFunction units
yComponent function = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2d.yComponent
          VectorBounds2d.yComponent
          function.compiled
  SurfaceFunction.new
    compiledYComponent
    (\parameter -> (derivative parameter function).yComponent)

components ::
  VectorSurfaceFunction2d units space ->
  (SurfaceFunction units, SurfaceFunction units)
components function = (xComponent function, yComponent function)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction2d units1 space ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction2d units3 space)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorSurfaceFunction2d units1 space ->
  SurfaceFunction units2 ->
  Result DivisionByZero (VectorSurfaceFunction2d (units1 ?/? units2) space)
quotient_ numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = SurfaceFunction.derivative p denominator
        let denominator'' = SurfaceFunction.derivative p denominator'
        let value = unsafeQuotient_ numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient_
                  (numerator'' ?*? denominator' .-. numerator' ?*? denominator'')
                  (2 *. SurfaceFunction.squared_ denominator')
        (value, firstDerivative)
  SurfaceFunction.Quotient.impl unsafeQuotient_ lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorSurfaceFunction2d units1 space ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d units3 space
unsafeQuotient lhs rhs = Units.specialize (unsafeQuotient_ lhs rhs)

unsafeQuotient_ ::
  VectorSurfaceFunction2d units1 space ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d (units1 ?/? units2) space
unsafeQuotient_ lhs rhs = do
  let quotientDerivative self p =
        unsafeQuotient_ (derivative p lhs) rhs
          .-. self .*. SurfaceFunction.unsafeQuotient (SurfaceFunction.derivative p rhs) rhs
  recursive
    (CompiledFunction.map2 (?/?) (?/?) (?/?) lhs.compiled rhs.compiled)
    quotientDerivative

squaredMagnitude_ :: VectorSurfaceFunction2d units space -> SurfaceFunction (units ?*? units)
squaredMagnitude_ function = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector2d.squaredMagnitude_
          VectorBounds2d.squaredMagnitude_
          function.compiled
  SurfaceFunction.new
    compiledSquaredMagnitude
    (\p -> 2 *. function `dot_` derivative p function)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction2d units1 space ->
  SurfaceFunction units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: Tolerance units => VectorSurfaceFunction2d units space -> SurfaceFunction units
magnitude function = SurfaceFunction.sqrt_ (squaredMagnitude_ function)

data IsZero = IsZero deriving (Eq, Show)

direction ::
  Tolerance units =>
  VectorSurfaceFunction2d units space ->
  Result IsZero (DirectionSurfaceFunction2d space)
direction function = case quotient function (magnitude function) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedFunction -> Ok (DirectionSurfaceFunction2d.unsafe normalizedFunction)
