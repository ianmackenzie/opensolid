module OpenSolid.SurfaceFunction3D
  ( SurfaceFunction3D
  , Compiled
  , Segment
  , BisectionTree
  , new
  , unsafe
  , displacedFrom
  , pointAt
  , pointOn
  , range
  , compiled
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivatives
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , degenerateLeft
  , degenerateRight
  , degenerateBottom
  , degenerateTop
  , nondegenerate
  , bisectionTree
  , normalDirectionRange
  , placeIn
  , relativeTo
  , transformBy
  , displaceBy
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval qualified as Interval
import OpenSolid.IsDegenerate (IsDegenerate (IsDegenerate))
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Pair qualified as Pair
import OpenSolid.PartialDerivatives qualified as PartialDerivatives
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2D (Region2D)
import {-# SOURCE #-} OpenSolid.Surface3D (Surface3D)
import {-# SOURCE #-} OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D.Nondegenerate qualified as SurfaceFunction3D.Nondegenerate
import OpenSolid.SurfaceFunction3D.Segment (Segment)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data SurfaceFunction3D space = SurfaceFunction3D
  { compiled :: Compiled space
  , partialDerivatives ::
      ( VectorSurfaceFunction3D Meters space
      , VectorSurfaceFunction3D Meters space
      )
  , maxSampledInteriorDivergence :: ~Length
  , degenerateLeft :: ~Bool
  , degenerateRight :: ~Bool
  , degenerateBottom :: ~Bool
  , degenerateTop :: ~Bool
  , bisectionTree :: Nondegenerate.Field (BisectionTree space)
  }

type Compiled space =
  CompiledFunction UvPoint (Point3D space) UvBounds (Bounds3D space)

type BisectionTree space = Bisection.Tree UvBounds (Segment space)

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3D space1)
    (SurfaceFunction3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  f - g =
    VectorSurfaceFunction3D.new
      (compiled f - compiled g)
      (Pair.map2 (-) (partialDerivatives f) (partialDerivatives g))

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3D space1)
    (Point3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  function - point =
    VectorSurfaceFunction3D.new
      (compiled function - CompiledFunction.constant point)
      (partialDerivatives function)

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (SurfaceFunction3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  point - function =
    VectorSurfaceFunction3D.new
      (CompiledFunction.constant point - compiled function)
      (Pair.map negate (partialDerivatives function))

instance
  Composition
    ()
    (SurfaceFunction3D space)
    (Region2D Unitless)
    (Surface3D space)
  where
  function << domain = Surface3D.parametric function domain

instance
  Composition
    ()
    (SurfaceFunction3D space)
    (SurfaceFunction2D Unitless)
    (SurfaceFunction3D space)
  where
  f << g = do
    let (dfdx, dfdy) = Pair.map (<< g) (partialDerivatives f)
    let (dgdu, dgdv) = SurfaceFunction2D.partialDerivatives g
    let (dxdu, dydu) = VectorSurfaceFunction2D.components dgdu
    let (dxdv, dydv) = VectorSurfaceFunction2D.components dgdv
    let compiledComposed = compiled f << SurfaceFunction2D.compiled g
    let composedPartialDerivatives =
          ( dfdx * dxdu + dfdy * dydu
          , dfdx * dxdv + dfdy * dydv
          )
    unsafe compiledComposed composedPartialDerivatives

new ::
  Tolerance Meters =>
  Compiled space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space) ->
  Result (IsDegenerate ()) (SurfaceFunction3D space)
new givenCompiled givenPartialDerivatives = do
  let candidate = unsafe givenCompiled givenPartialDerivatives
  if candidate.maxSampledInteriorDivergence ~= Length.zero
    then Err (IsDegenerate ())
    else Ok candidate

unsafe ::
  Compiled space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space) ->
  SurfaceFunction3D space
unsafe givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge
          VectorSurfaceFunction3D.new
          VectorSurfaceFunction3D.compiled
          VectorSurfaceFunction3D.partialDerivatives
          givenPartialDerivatives
  recursive \result -> do
    let maxSampledInteriorDivergence =
          NonEmpty.maximumOf (divergence result) UvPoint.interiorSamples
    let degeneracyTolerance = Tolerance.unitless * maxSampledInteriorDivergence
    let degenerateEdge samplePoints = do
          let maxSampledDivergence = NonEmpty.maximumOf (divergence result) samplePoints
          Tolerance.using degeneracyTolerance (maxSampledDivergence ~= Length.zero)
    SurfaceFunction3D
      { compiled = givenCompiled
      , partialDerivatives = mergedPartialDerivatives
      , maxSampledInteriorDivergence
      , degenerateLeft = degenerateEdge UvPoint.leftSamples
      , degenerateRight = degenerateEdge UvPoint.rightSamples
      , degenerateBottom = degenerateEdge UvPoint.bottomSamples
      , degenerateTop = degenerateEdge UvPoint.topSamples
      , bisectionTree = Nondegenerate.field (buildBisectionTree UvBounds.unitSquare) result
      }

buildBisectionTree :: UvBounds -> Nondegenerate (SurfaceFunction3D space) -> BisectionTree space
buildBisectionTree uvRange function = do
  let segment = SurfaceFunction3D.Nondegenerate.segment uvRange function
  let UvBounds uRange vRange = uvRange
  let (uLeft, uRight) = Interval.bisect uRange
  let (vBottom, vTop) = Interval.bisect vRange
  let bottomLeft = buildBisectionTree (UvBounds uLeft vBottom) function
  let bottomRight = buildBisectionTree (UvBounds uRight vBottom) function
  let topLeft = buildBisectionTree (UvBounds uLeft vTop) function
  let topRight = buildBisectionTree (UvBounds uRight vTop) function
  let children = NonEmpty.four bottomLeft bottomRight topLeft topRight
  Bisection.Tree uvRange segment children

displacedFrom :: Point3D space -> VectorSurfaceFunction3D Meters space -> SurfaceFunction3D space
displacedFrom point displacementFunction =
  unsafe
    (CompiledFunction.constant point + VectorSurfaceFunction3D.compiled displacementFunction)
    (VectorSurfaceFunction3D.partialDerivatives displacementFunction)

divergence :: SurfaceFunction3D space -> UvPoint -> Length
divergence function uvPoint = do
  let (duValue, dvValue) = partialDerivativesAt uvPoint function
  Vector3D.divergence duValue dvValue

{-# INLINE pointAt #-}
pointAt :: UvPoint -> SurfaceFunction3D space -> Point3D space
pointAt uvPoint function = CompiledFunction.value uvPoint function.compiled

{-# INLINE pointOn #-}
pointOn :: SurfaceFunction3D space -> UvPoint -> Point3D space
pointOn function uvPoint = pointAt uvPoint function

{-# INLINE range #-}
range :: UvBounds -> SurfaceFunction3D space -> Bounds3D space
range uvRange function = CompiledFunction.range uvRange function.compiled

partialDerivativesAt ::
  UvPoint ->
  SurfaceFunction3D space ->
  (Vector3D Meters space, Vector3D Meters space)
partialDerivativesAt uvPoint function =
  Pair.map (VectorSurfaceFunction3D.valueAt uvPoint) (partialDerivatives function)

partialDerivativeRanges ::
  UvBounds ->
  SurfaceFunction3D space ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space)
partialDerivativeRanges uvRange function =
  Pair.map (VectorSurfaceFunction3D.range uvRange) (partialDerivatives function)

secondPartialDerivativesAt ::
  UvPoint ->
  SurfaceFunction3D space ->
  (Vector3D Meters space, Vector3D Meters space, Vector3D Meters space)
secondPartialDerivativesAt uvPoint function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  let fuuValue = VectorSurfaceFunction3D.valueAt uvPoint fuu
  let fuvValue = VectorSurfaceFunction3D.valueAt uvPoint fuv
  let fvvValue = VectorSurfaceFunction3D.valueAt uvPoint fvv
  (fuuValue, fuvValue, fvvValue)

secondPartialDerivativeRanges ::
  UvBounds ->
  SurfaceFunction3D space ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space, VectorBounds3D Meters space)
secondPartialDerivativeRanges uvRange function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  let fuuRange = VectorSurfaceFunction3D.range uvRange fuu
  let fuvRange = VectorSurfaceFunction3D.range uvRange fuv
  let fvvRange = VectorSurfaceFunction3D.range uvRange fvv
  (fuuRange, fuvRange, fvvRange)

{-# INLINE compiled #-}
compiled :: SurfaceFunction3D space -> Compiled space
compiled = (.compiled)

{-# INLINE partialDerivatives #-}
partialDerivatives ::
  SurfaceFunction3D space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space)
partialDerivatives = (.partialDerivatives)

secondPartialDerivatives ::
  SurfaceFunction3D space ->
  ( VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  )
secondPartialDerivatives function = do
  let (fu, fv) = partialDerivatives function
  let (fuu, fuv) = VectorSurfaceFunction3D.partialDerivatives fu
  let (_, fvv) = VectorSurfaceFunction3D.partialDerivatives fv
  (fuu, fuv, fvv)

degenerateLeft :: SurfaceFunction3D space -> Bool
degenerateLeft = (.degenerateLeft)

degenerateRight :: SurfaceFunction3D space -> Bool
degenerateRight = (.degenerateRight)

degenerateBottom :: SurfaceFunction3D space -> Bool
degenerateBottom = (.degenerateBottom)

degenerateTop :: SurfaceFunction3D space -> Bool
degenerateTop = (.degenerateTop)

nondegenerate ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  Result (IsDegenerate ()) (Nondegenerate (SurfaceFunction3D space))
nondegenerate function =
  if function.maxSampledInteriorDivergence ~= Length.zero
    then Err (IsDegenerate ())
    else Ok (Nondegenerate function)

bisectionTree :: Nondegenerate (SurfaceFunction3D space) -> BisectionTree space
bisectionTree = Nondegenerate.get (.bisectionTree)

normalDirectionRange ::
  Tolerance Meters =>
  UvBounds ->
  SurfaceFunction3D space ->
  DirectionBounds3D space
normalDirectionRange uvRange function = do
  let (fu, fv) = partialDerivatives function
  let fuDirectionBounds = VectorSurfaceFunction3D.directionRange uvRange fu
  let fvDirectionBounds = VectorSurfaceFunction3D.directionRange uvRange fv
  VectorBounds3D.direction (fuDirectionBounds `cross` fvDirectionBounds)

transformBy ::
  Transform.Tag.IsOrthonormal tag =>
  Transform3D tag space ->
  SurfaceFunction3D space ->
  SurfaceFunction3D space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point3D.transformBy transform)
          (Bounds3D.transformBy transform)
          function.compiled
  let transformDerivative =
        VectorSurfaceFunction3D.transformBy (Transform3D.vectorTransform transform)
  let transformedDerivatives =
        Pair.map transformDerivative (partialDerivatives function)
  unsafe compiledTransformed transformedDerivatives

placeIn :: Frame3D global local -> SurfaceFunction3D local -> SurfaceFunction3D global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point3D.placeIn frame)
          (Bounds3D.placeIn frame)
          function.compiled
  let placedPartialDerivatives =
        Pair.map (VectorSurfaceFunction3D.placeIn frame) (partialDerivatives function)
  unsafe compiledPlaced placedPartialDerivatives

relativeTo :: Frame3D global local -> SurfaceFunction3D global -> SurfaceFunction3D local
relativeTo frame = placeIn (Frame3D.inverse frame)

displaceBy ::
  VectorSurfaceFunction3D Meters space ->
  SurfaceFunction3D space ->
  SurfaceFunction3D space
displaceBy displacementFunction surfaceFunction = do
  let compiledOffset =
        compiled surfaceFunction + VectorSurfaceFunction3D.compiled displacementFunction
  let compiledPartialDerivatives =
        Pair.map2
          (+)
          (partialDerivatives surfaceFunction)
          (VectorSurfaceFunction3D.partialDerivatives displacementFunction)
  unsafe compiledOffset compiledPartialDerivatives
