{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorSurfaceFunction3D
  ( VectorSurfaceFunction3D
  , Compiled
  , new
  , isZero
  , singularU0
  , singularU1
  , singularV0
  , singularV1
  , nondegenerate
  , desingularize
  , desingularized
  , zero
  , constant
  , value
  , bounds
  , compiled
  , derivative
  , derivativeValue
  , derivativeBounds
  , placeIn
  , relativeTo
  , transformBy
  , quotient
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , direction
  , directionBounds
  , newtonRaphson
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D (DirectionSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D qualified as DirectionSurfaceFunction3D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.NewtonRaphson3D qualified as NewtonRaphson3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Blending qualified as SurfaceFunction1D.Blending
import OpenSolid.SurfaceFunction1D.Quotient qualified as SurfaceFunction1D.Quotient
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

data VectorSurfaceFunction3D units space
  = VectorSurfaceFunction3D
  { compiled :: Compiled units space
  , du :: ~(VectorSurfaceFunction3D units space)
  , dv :: ~(VectorSurfaceFunction3D units space)
  , maxSampledMagnitude :: Quantity units
  , singularU0 :: ~Bool
  , singularU1 :: ~Bool
  , singularV0 :: ~Bool
  , singularV1 :: ~Bool
  }

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector3D units space)
    UvBounds
    (VectorBounds3D units space)

instance HasUnits (VectorSurfaceFunction3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorSurfaceFunction3D units1 space1) (VectorSurfaceFunction3D units2 space2)
  where
  coerce function =
    VectorSurfaceFunction3D
      { compiled = Units.coerce function.compiled
      , du = Units.coerce function.du
      , dv = Units.coerce function.dv
      , maxSampledMagnitude = Units.coerce function.maxSampledMagnitude
      , singularU0 = function.singularU0
      , singularU1 = function.singularU1
      , singularV0 = function.singularV0
      , singularV1 = function.singularV1
      }

instance Negation (VectorSurfaceFunction3D units space) where
  negate function = new (negate function.compiled) (\p -> negate (derivative p function))

instance
  Multiplication
    Sign
    (VectorSurfaceFunction3D units space)
    (VectorSurfaceFunction3D units space)
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction3D units space)
    Sign
    (VectorSurfaceFunction3D units space)
  where
  function * Positive = function
  function * Negative = -function

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (\p -> derivative p lhs + derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  f + v = f + constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  v + f = constant v + f

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (\p -> derivative p lhs - derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  f - v = f - constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units1 space1)
  where
  v - f = constant v - f

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Point3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  point + function = SurfaceFunction3D.constant point + function

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Point3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  point - function = SurfaceFunction3D.constant point - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> SurfaceFunction1D.derivative p lhs ?*? rhs + lhs ?*? derivative p rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorSurfaceFunction3D units2 space)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  f1 ?*? f2 = SurfaceFunction1D.constant f1 ?*? f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction3D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> derivative p lhs ?*? rhs + lhs ?*? SurfaceFunction1D.derivative p rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction3D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  function ?*? quantity = function ?*? SurfaceFunction1D.constant quantity

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorSurfaceFunction3D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorSurfaceFunction3D units1 space)
    (Quantity units2)
    (VectorSurfaceFunction3D (units1 ?/? units2) space)
  where
  function ?/? quantity = Units.simplify (function ?*? (1.0 ?/? quantity))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space1)
  where
  lhs `cross_` rhs =
    new
      (lhs.compiled `cross_` rhs.compiled)
      (\p -> derivative p lhs `cross_` rhs + lhs `cross_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (VectorSurfaceFunction3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space1)
  where
  f `cross_` v = f `cross_` constant v

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space1)
  where
  v `cross_` f = constant v `cross_` f

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction3D units space1)
    (Direction3D space2)
    (VectorSurfaceFunction3D units space1)
  where
  lhs `cross` rhs = lhs `cross` Vector3D.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (VectorSurfaceFunction3D units space2)
    (VectorSurfaceFunction3D units space2)
  where
  lhs `cross` rhs = Vector3D.unit lhs `cross` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    SurfaceFunction1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (\p -> derivative p lhs `dot_` rhs + lhs `dot_` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorSurfaceFunction3D units1 space1)
    (Vector3D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function `dot_` vector = function `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3D units1 space1)
    (VectorSurfaceFunction3D units2 space2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  vector `dot_` function = constant vector `dot_` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction3D units space1)
    (Direction3D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector3D.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction3D space1)
    (VectorSurfaceFunction3D units space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = Vector3D.unit lhs `dot` rhs

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction3D units space) ->
  VectorSurfaceFunction3D units space
new givenCompiled derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' =
        VectorSurfaceFunction3D
          { compiled = dv.compiled
          , du = du.dv
          , dv = dv.dv
          , maxSampledMagnitude = dv.maxSampledMagnitude
          , singularU0 = dv.singularU0
          , singularU1 = dv.singularU1
          , singularV0 = dv.singularV0
          , singularV1 = dv.singularV1
          }
  let sampledMagnitude uvPoint = Vector3D.magnitude (CompiledFunction.value givenCompiled uvPoint)
  let maxSampledMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.samples
  VectorSurfaceFunction3D
    { compiled = givenCompiled
    , du = du
    , dv = dv'
    , maxSampledMagnitude = maxSampledMagnitude
    , singularU0 = boundaryIsSingular U 0.0 givenCompiled maxSampledMagnitude
    , singularU1 = boundaryIsSingular U 1.0 givenCompiled maxSampledMagnitude
    , singularV0 = boundaryIsSingular V 0.0 givenCompiled maxSampledMagnitude
    , singularV1 = boundaryIsSingular V 1.0 givenCompiled maxSampledMagnitude
    }

boundaryIsSingular :: SurfaceParameter -> Number -> Compiled units space -> Quantity units -> Bool
boundaryIsSingular parameter parameterValue compiledFunction maxSampledMagnitude =
  Tolerance.using (Tolerance.unitless * maxSampledMagnitude) do
    let isZeroAt testPoint = CompiledFunction.value compiledFunction testPoint ~= Vector3D.zero
    NonEmpty.all isZeroAt (testPoints parameter parameterValue)

testPoints :: SurfaceParameter -> Number -> NonEmpty UvPoint
testPoints U uValue = NonEmpty.map (\v -> UvPoint uValue v) Parameter.samples
testPoints V vValue = NonEmpty.map (\u -> UvPoint u vValue) Parameter.samples

isZero :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
isZero function = function.maxSampledMagnitude <= ?tolerance

singularU0 :: VectorSurfaceFunction3D units space -> Bool
singularU0 = (.singularU0)

singularU1 :: VectorSurfaceFunction3D units space -> Bool
singularU1 = (.singularU1)

singularV0 :: VectorSurfaceFunction3D units space -> Bool
singularV0 = (.singularV0)

singularV1 :: VectorSurfaceFunction3D units space -> Bool
singularV1 = (.singularV1)

nondegenerate ::
  Tolerance units =>
  VectorSurfaceFunction3D units space ->
  Result IsDegenerate (Nondegenerate (VectorSurfaceFunction3D units space))
nondegenerate function = if isZero function then Error IsDegenerate else Ok (Nondegenerate function)

desingularize ::
  VectorSurfaceFunction3D units space ->
  "singularityU0"
    ::: Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityU1"
    ::: Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityV0"
    ::: Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityV1"
    ::: Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  VectorSurfaceFunction3D units space
desingularize = SurfaceFunction1D.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction1D Unitless ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

zero :: VectorSurfaceFunction3D units space
zero = constant Vector3D.zero

constant :: Vector3D units space -> VectorSurfaceFunction3D units space
constant vector = new (CompiledFunction.constant vector) (const zero)

value :: VectorSurfaceFunction3D units space -> UvPoint -> Vector3D units space
value function uvPoint = CompiledFunction.value function.compiled uvPoint

bounds :: VectorSurfaceFunction3D units space -> UvBounds -> VectorBounds3D units space
bounds function uvBounds = CompiledFunction.bounds function.compiled uvBounds

compiled :: VectorSurfaceFunction3D units space -> Compiled units space
compiled = (.compiled)

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space
derivative U = (.du)
derivative V = (.dv)

derivativeValue ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  UvPoint ->
  Vector3D units space
derivativeValue U function uvPoint = value function.du uvPoint
derivativeValue V function uvPoint = value function.dv uvPoint

derivativeBounds ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  UvBounds ->
  VectorBounds3D units space
derivativeBounds U function uvBounds = bounds function.du uvBounds
derivativeBounds V function uvBounds = bounds function.dv uvBounds

placeIn ::
  Frame3D global local ->
  VectorSurfaceFunction3D units local ->
  VectorSurfaceFunction3D units global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector3D.placeIn frame)
          (VectorBounds3D.placeIn frame)
          function.compiled
  new compiledPlaced (\p -> placeIn frame (derivative p function))

relativeTo ::
  Frame3D global local ->
  VectorSurfaceFunction3D units global ->
  VectorSurfaceFunction3D units local
relativeTo frame function = placeIn (Frame3D.inverse frame) function

transformBy ::
  Transform3D tag space ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector3D.transformBy transform)
          (VectorBounds3D.transformBy transform)
          function.compiled
  new compiledTransformed (\p -> transformBy transform (derivative p function))

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (VectorSurfaceFunction3D units3 space)
quotient lhs rhs = Result.map Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (VectorSurfaceFunction3D (units1 ?/? units2) space)
quotient_ numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = SurfaceFunction1D.derivative p denominator
        let denominator'' = SurfaceFunction1D.derivative p denominator'
        let lhopitalSurface = unsafeQuotient_ numerator' denominator'
        let lhopitalDerivative =
              Units.simplify $
                unsafeQuotient_
                  (numerator'' ?*? denominator' - numerator' ?*? denominator'')
                  (2.0 * SurfaceFunction1D.squared_ denominator')
        (lhopitalSurface, lhopitalDerivative)
  SurfaceFunction1D.Quotient.impl unsafeQuotient_ lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2 ->
  VectorSurfaceFunction3D units3 space
unsafeQuotient lhs rhs = Units.specialize (unsafeQuotient_ lhs rhs)

unsafeQuotient_ ::
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2 ->
  VectorSurfaceFunction3D (units1 ?/? units2) space
unsafeQuotient_ lhs rhs = do
  let compiledQuotient = CompiledFunction.map2 (?/?) (?/?) (?/?) lhs.compiled rhs.compiled
  recursive \self -> do
    let quotientDerivative p =
          unsafeQuotient_ (derivative p lhs) rhs
            - self * SurfaceFunction1D.unsafeQuotient (SurfaceFunction1D.derivative p rhs) rhs
    new compiledQuotient quotientDerivative

squaredMagnitude_ :: VectorSurfaceFunction3D units space -> SurfaceFunction1D (units ?*? units)
squaredMagnitude_ function = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector3D.squaredMagnitude_
          VectorBounds3D.squaredMagnitude_
          function.compiled
  SurfaceFunction1D.new
    compiledSquaredMagnitude
    (\p -> 2.0 * function `dot_` derivative p function)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: Tolerance units => VectorSurfaceFunction3D units space -> SurfaceFunction1D units
magnitude function = SurfaceFunction1D.sqrt_ (squaredMagnitude_ function)

direction ::
  Tolerance units =>
  VectorSurfaceFunction3D units space ->
  Result IsDegenerate (DirectionSurfaceFunction3D space)
direction function = case quotient function (magnitude function) of
  Error DivisionByZero -> Error IsDegenerate
  Ok normalizedFunction -> Ok (DirectionSurfaceFunction3D.unsafe normalizedFunction)

directionBounds ::
  VectorSurfaceFunction3D units space ->
  UvBounds ->
  DirectionBounds3D space
directionBounds function uvBounds = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvBounds
  DirectionBounds3D.unsafe $
    VectorBounds3D.normalize $
      if
        | uLow == 0.0 && function.singularU0 -> bounds function.du uvBounds
        | uHigh == 1.0 && function.singularU1 -> negate (bounds function.du uvBounds)
        | vLow == 0.0 && function.singularV0 -> bounds function.dv uvBounds
        | vHigh == 1.0 && function.singularV1 -> negate (bounds function.dv uvBounds)
        | otherwise -> bounds function uvBounds

newtonRaphson :: VectorSurfaceFunction3D units space -> UvPoint -> UvPoint
newtonRaphson function uvPoint0 = do
  let uDerivative = derivative U function
  let vDerivative = derivative V function
  let evaluate uvPoint =
        (# value function uvPoint, value uDerivative uvPoint, value vDerivative uvPoint #)
  NewtonRaphson3D.surface evaluate uvPoint0
