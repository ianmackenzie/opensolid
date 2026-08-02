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
  , valueAt
  , valueOf
  , range
  , compiled
  , partialDerivatives
  , secondPartialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
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
import OpenSolid.Pair qualified as Pair
import OpenSolid.PartialDerivatives qualified as PartialDerivatives
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
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
  , partialDerivatives :: (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space)
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
      , partialDerivatives = Pair.map Units.coerce function.partialDerivatives
      , maxSampledInteriorMagnitude = Units.coerce function.maxSampledInteriorMagnitude
      , maxSampledLeftMagnitude = Units.coerce function.maxSampledLeftMagnitude
      , maxSampledRightMagnitude = Units.coerce function.maxSampledRightMagnitude
      , maxSampledBottomMagnitude = Units.coerce function.maxSampledBottomMagnitude
      , maxSampledTopMagnitude = Units.coerce function.maxSampledTopMagnitude
      }

instance Negation (VectorSurfaceFunction3D units space) where
  negate function = new (negate function.compiled) (Pair.map negate function.partialDerivatives)

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
  f + g =
    new
      (compiled f + compiled g)
      (Pair.map2 (+) (partialDerivatives f) (partialDerivatives g))

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
  f - g =
    new
      (compiled f - compiled g)
      (Pair.map2 (-) (partialDerivatives f) (partialDerivatives g))

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
  f ?*? g = do
    let (dfdu, dfdv) = SurfaceFunction1D.partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let compiledProduct = SurfaceFunction1D.compiled f ?*? compiled g
    let productPartialDerivatives =
          ( dfdu ?*? g + f ?*? dgdu
          , dfdv ?*? g + f ?*? dgdv
          )
    new compiledProduct productPartialDerivatives

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
  f ?*? g = do
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = SurfaceFunction1D.partialDerivatives g
    let compiledProduct = compiled f ?*? SurfaceFunction1D.compiled g
    let productPartialDerivatives =
          ( dfdu ?*? g + f ?*? dgdu
          , dfdv ?*? g + f ?*? dgdv
          )
    new compiledProduct productPartialDerivatives

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
  f `cross_` g = do
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let compiledCrossProduct = compiled f `cross_` compiled g
    let crossProductPartialDerivatives =
          ( dfdu `cross_` g + f `cross_` dgdu
          , dfdv `cross_` g + f `cross_` dgdv
          )
    new compiledCrossProduct crossProductPartialDerivatives

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
  f `dot_` g = do
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let compiledDotProduct = compiled f `dot_` compiled g
    let dotProductPartialDerivatives =
          ( dfdu `dot_` g + f `dot_` dgdu
          , dfdv `dot_` g + f `dot_` dgdv
          )
    SurfaceFunction1D.new compiledDotProduct dotProductPartialDerivatives

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
  (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space) ->
  VectorSurfaceFunction3D units space
new givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge new compiled partialDerivatives givenPartialDerivatives
  let sampledMagnitude uvPoint = Vector3D.magnitude (CompiledFunction.value uvPoint givenCompiled)
  VectorSurfaceFunction3D
    { compiled = givenCompiled
    , partialDerivatives = mergedPartialDerivatives
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
constant vector = new (CompiledFunction.constant vector) (zero, zero)

{-# INLINE valueAt #-}
valueAt :: UvPoint -> VectorSurfaceFunction3D units space -> Vector3D units space
valueAt uvPoint function = CompiledFunction.value uvPoint function.compiled

{-# INLINE valueOf #-}
valueOf :: VectorSurfaceFunction3D units space -> UvPoint -> Vector3D units space
valueOf function uvPoint = valueAt uvPoint function

{-# INLINE range #-}
range :: UvBounds -> VectorSurfaceFunction3D units space -> VectorBounds3D units space
range uvRange function = CompiledFunction.range uvRange function.compiled

{-# INLINE compiled #-}
compiled :: VectorSurfaceFunction3D units space -> Compiled units space
compiled = (.compiled)

{-# INLINE partialDerivatives #-}
partialDerivatives ::
  VectorSurfaceFunction3D units space ->
  (VectorSurfaceFunction3D units space, VectorSurfaceFunction3D units space)
partialDerivatives = (.partialDerivatives)

secondPartialDerivatives ::
  VectorSurfaceFunction3D units space ->
  ( VectorSurfaceFunction3D units space
  , VectorSurfaceFunction3D units space
  , VectorSurfaceFunction3D units space
  )
secondPartialDerivatives function = do
  let (fu, fv) = partialDerivatives function
  let (fuu, fuv) = partialDerivatives fu
  let (_, fvv) = partialDerivatives fv
  (fuu, fuv, fvv)

partialDerivativesAt ::
  UvPoint ->
  VectorSurfaceFunction3D units space ->
  (Vector3D units space, Vector3D units space)
partialDerivativesAt uvPoint function = do
  let (fu, fv) = partialDerivatives function
  (valueAt uvPoint fu, valueAt uvPoint fv)

partialDerivativeRanges ::
  UvBounds ->
  VectorSurfaceFunction3D units space ->
  (VectorBounds3D units space, VectorBounds3D units space)
partialDerivativeRanges uvRange function = do
  let (fu, fv) = partialDerivatives function
  (range uvRange fu, range uvRange fv)

secondPartialDerivativesAt ::
  UvPoint ->
  VectorSurfaceFunction3D units space ->
  (Vector3D units space, Vector3D units space, Vector3D units space)
secondPartialDerivativesAt uvPoint function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  (valueAt uvPoint fuu, valueAt uvPoint fuv, valueAt uvPoint fvv)

secondPartialDerivativeRanges ::
  UvBounds ->
  VectorSurfaceFunction3D units space ->
  (VectorBounds3D units space, VectorBounds3D units space, VectorBounds3D units space)
secondPartialDerivativeRanges uvRange function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  (range uvRange fuu, range uvRange fuv, range uvRange fvv)

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
  let placedPartialDerivatives = Pair.map (placeIn frame) function.partialDerivatives
  new compiledPlaced placedPartialDerivatives

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
  let transformedPartialDerivatives = Pair.map (transformBy transform) function.partialDerivatives
  new compiledTransformed transformedPartialDerivatives

squaredMagnitude_ :: VectorSurfaceFunction3D units space -> SurfaceFunction1D (units ?*? units)
squaredMagnitude_ function = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector3D.squaredMagnitude_
          VectorBounds3D.squaredMagnitude_
          function.compiled
  let squaredMagnitudePartialDerivatives =
        Pair.map (2.0 * function `dot_`) function.partialDerivatives
  SurfaceFunction1D.new compiledSquaredMagnitude squaredMagnitudePartialDerivatives

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction3D units1 space ->
  SurfaceFunction1D units2
squaredMagnitude = Units.specialize . squaredMagnitude_

directionRange ::
  Tolerance units =>
  UvBounds ->
  VectorSurfaceFunction3D units space ->
  DirectionBounds3D space
directionRange uvRange function = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let (fuRange, fvRange) = partialDerivativeRanges uvRange function
  VectorBounds3D.direction $
    if
      | uLow == 0.0 && degenerateLeft function -> fuRange
      | uHigh == 1.0 && degenerateRight function -> negate fuRange
      | vLow == 0.0 && degenerateBottom function -> fvRange
      | vHigh == 1.0 && degenerateTop function -> negate fvRange
      | otherwise -> range uvRange function

newtonRaphson :: VectorSurfaceFunction3D units space -> UvPoint -> Fuzzy UvPoint
newtonRaphson f uvPoint0 = do
  let (fu, fv) = partialDerivatives f
  let evaluate uvPoint = (# valueAt uvPoint f, valueAt uvPoint fu, valueAt uvPoint fv #)
  NewtonRaphson.Surface.solveFrom uvPoint0 evaluate
