{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorSurfaceFunction3D
  ( VectorSurfaceFunction3D
  , Compiled
  , new
  , isZero
  , degenerateLeft
  , degenerateRight
  , degenerateBottom
  , degenerateTop
  , nondegenerate
  , zero
  , constant
  , value
  , range
  , compiled
  , derivative
  , derivativeValue
  , derivativeRange
  , placeIn
  , relativeTo
  , transformBy
  , squaredMagnitude
  , squaredMagnitude_
  , directionRange
  , newtonRaphson
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorTransform3D (VectorTransform3D)

data VectorSurfaceFunction3D units space
  = VectorSurfaceFunction3D
  { compiled :: Compiled units space
  , du :: ~(VectorSurfaceFunction3D units space)
  , dv :: ~(VectorSurfaceFunction3D units space)
  , maxSampledInteriorMagnitude :: ~(Quantity units)
  , maxSampledLeftMagnitude :: ~(Quantity units)
  , maxSampledRightMagnitude :: ~(Quantity units)
  , maxSampledBottomMagnitude :: ~(Quantity units)
  , maxSampledTopMagnitude :: ~(Quantity units)
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
      , maxSampledInteriorMagnitude = Units.coerce function.maxSampledInteriorMagnitude
      , maxSampledLeftMagnitude = Units.coerce function.maxSampledLeftMagnitude
      , maxSampledRightMagnitude = Units.coerce function.maxSampledRightMagnitude
      , maxSampledBottomMagnitude = Units.coerce function.maxSampledBottomMagnitude
      , maxSampledTopMagnitude = Units.coerce function.maxSampledTopMagnitude
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
      (SurfaceFunction1D.compiled lhs ?*? rhs.compiled)
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
      (lhs.compiled ?*? SurfaceFunction1D.compiled rhs)
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
          , du = derivative V du
          , dv = derivative V dv
          , maxSampledInteriorMagnitude = dv.maxSampledInteriorMagnitude
          , maxSampledLeftMagnitude = dv.maxSampledLeftMagnitude
          , maxSampledRightMagnitude = dv.maxSampledRightMagnitude
          , maxSampledBottomMagnitude = dv.maxSampledBottomMagnitude
          , maxSampledTopMagnitude = dv.maxSampledTopMagnitude
          }
  let sampledMagnitude uvPoint = Vector3D.magnitude (CompiledFunction.value givenCompiled uvPoint)
  VectorSurfaceFunction3D
    { compiled = givenCompiled
    , du = du
    , dv = dv'
    , maxSampledInteriorMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.interiorSamples
    , maxSampledLeftMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.leftSamples
    , maxSampledRightMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.rightSamples
    , maxSampledBottomMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.bottomSamples
    , maxSampledTopMagnitude = NonEmpty.maximumOf sampledMagnitude UvPoint.topSamples
    }

isZero :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
isZero function = function.maxSampledInteriorMagnitude ~= Quantity.zero

degenerateLeft :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
degenerateLeft function = function.maxSampledLeftMagnitude ~= Quantity.zero

degenerateRight :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
degenerateRight function = function.maxSampledRightMagnitude ~= Quantity.zero

degenerateBottom :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
degenerateBottom function = function.maxSampledBottomMagnitude ~= Quantity.zero

degenerateTop :: Tolerance units => VectorSurfaceFunction3D units space -> Bool
degenerateTop function = function.maxSampledTopMagnitude ~= Quantity.zero

nondegenerate ::
  Tolerance units =>
  VectorSurfaceFunction3D units space ->
  Result IsDegenerate (Nondegenerate (VectorSurfaceFunction3D units space))
nondegenerate function = if isZero function then Err IsDegenerate else Ok (Nondegenerate function)

zero :: VectorSurfaceFunction3D units space
zero = constant Vector3D.zero

constant :: Vector3D units space -> VectorSurfaceFunction3D units space
constant vector = new (CompiledFunction.constant vector) (const zero)

value :: VectorSurfaceFunction3D units space -> UvPoint -> Vector3D units space
value function uvPoint = CompiledFunction.value function.compiled uvPoint

range :: VectorSurfaceFunction3D units space -> UvBounds -> VectorBounds3D units space
range function uvRange = CompiledFunction.range function.compiled uvRange

{-# INLINE compiled #-}
compiled :: VectorSurfaceFunction3D units space -> Compiled units space
compiled = (.compiled)

{-# INLINE derivative #-}
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
derivativeValue U function uvPoint = value (derivative U function) uvPoint
derivativeValue V function uvPoint = value (derivative V function) uvPoint

derivativeRange ::
  SurfaceParameter ->
  VectorSurfaceFunction3D units space ->
  UvBounds ->
  VectorBounds3D units space
derivativeRange U function uvRange = range (derivative U function) uvRange
derivativeRange V function uvRange = range (derivative V function) uvRange

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
  VectorTransform3D tag space ->
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

directionRange ::
  Tolerance units =>
  VectorSurfaceFunction3D units space -> UvBounds -> DirectionBounds3D space
directionRange function uvRange = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  VectorBounds3D.direction $
    if
      | uLow == 0.0 && degenerateLeft function -> range (derivative U function) uvRange
      | uHigh == 1.0 && degenerateRight function -> negate (range (derivative U function) uvRange)
      | vLow == 0.0 && degenerateBottom function -> range (derivative V function) uvRange
      | vHigh == 1.0 && degenerateTop function -> negate (range (derivative V function) uvRange)
      | otherwise -> range function uvRange

newtonRaphson :: VectorSurfaceFunction3D units space -> UvPoint -> Fuzzy UvPoint
newtonRaphson function uvPoint0 = do
  let uDerivative = derivative U function
  let vDerivative = derivative V function
  let evaluate uvPoint =
        (# value function uvPoint, value uDerivative uvPoint, value vDerivative uvPoint #)
  NewtonRaphson.Surface.solveFrom uvPoint0 evaluate
