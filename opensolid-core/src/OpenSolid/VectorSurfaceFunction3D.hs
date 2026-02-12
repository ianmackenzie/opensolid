module OpenSolid.VectorSurfaceFunction3D
  ( VectorSurfaceFunction3D
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
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , IsZero (IsZero)
  , direction
  )
where

import GHC.Records (HasField)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Direction3D (Direction3D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D (DirectionSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D qualified as DirectionSurfaceFunction3D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
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
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

data VectorSurfaceFunction3D units space
  = VectorSurfaceFunction3D
      (Compiled units space)
      ~(VectorSurfaceFunction3D units space)
      ~(VectorSurfaceFunction3D units space)

instance
  HasField
    "du"
    (VectorSurfaceFunction3D units space)
    (VectorSurfaceFunction3D units space)
  where
  getField (VectorSurfaceFunction3D _ du _) = du

instance
  HasField
    "dv"
    (VectorSurfaceFunction3D units space)
    (VectorSurfaceFunction3D units space)
  where
  getField (VectorSurfaceFunction3D _ _ dv) = dv

type Compiled units space =
  CompiledFunction
    UvPoint
    (Vector3D units space)
    UvBounds
    (VectorBounds3D units space)

instance HasUnits (VectorSurfaceFunction3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3D unitsA space1)
    (VectorSurfaceFunction3D unitsB space2)
  where
  coerce (VectorSurfaceFunction3D c du dv) =
    VectorSurfaceFunction3D (Units.coerce c) (Units.coerce du) (Units.coerce dv)

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
  function ?*? value = function ?*? SurfaceFunction1D.constant value

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
  function ?/? value = Units.simplify (function ?*? (1.0 ?/? value))

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

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3D units space)
    (Compiled units space)
  where
  getField (VectorSurfaceFunction3D c _ _) = c

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction3D units space) ->
  VectorSurfaceFunction3D units space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3D dv.compiled du.dv dv.dv
  VectorSurfaceFunction3D c du dv'

recursive ::
  Compiled units space ->
  ( VectorSurfaceFunction3D units space ->
    SurfaceParameter ->
    VectorSurfaceFunction3D units space
  ) ->
  VectorSurfaceFunction3D units space
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  VectorSurfaceFunction3D units space ->
  "singularityU0"
    # Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityU1"
    # Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityV0"
    # Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  "singularityV1"
    # Maybe (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
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
constant value = new (CompiledFunction.constant value) (const zero)

evaluate :: VectorSurfaceFunction3D units space -> UvPoint -> Vector3D units space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction3D units space ->
  UvBounds ->
  VectorBounds3D units space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  VectorSurfaceFunction3D units space
derivative U = (.du)
derivative V = (.dv)

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
        let value = unsafeQuotient_ numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient_
                  (numerator'' ?*? denominator' - numerator' ?*? denominator'')
                  (2.0 * SurfaceFunction1D.squared_ denominator')
        (value, firstDerivative)
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
  let quotientDerivative self p =
        unsafeQuotient_ (derivative p lhs) rhs
          - self * SurfaceFunction1D.unsafeQuotient (SurfaceFunction1D.derivative p rhs) rhs
  recursive
    (CompiledFunction.map2 (?/?) (?/?) (?/?) lhs.compiled rhs.compiled)
    quotientDerivative

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

data IsZero = IsZero deriving (Eq, Show)

direction ::
  Tolerance units =>
  VectorSurfaceFunction3D units space ->
  Result IsZero (DirectionSurfaceFunction3D space)
direction function = case quotient function (magnitude function) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedFunction -> Ok (DirectionSurfaceFunction3D.unsafe normalizedFunction)
