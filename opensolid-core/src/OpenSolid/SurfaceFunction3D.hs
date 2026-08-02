module OpenSolid.SurfaceFunction3D
  ( SurfaceFunction3D
  , Compiled
  , new
  , constant
  , pointAt
  , pointOn
  , range
  , compiled
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , nondegenerate
  , normalDirectionRange
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
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
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.UvBounds (UvBounds)
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
  , maxSampledNondegeneracy :: ~Length
  }

type Compiled space =
  CompiledFunction UvPoint (Point3D space) UvBounds (Bounds3D space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  f + g =
    new
      (compiled f + VectorSurfaceFunction3D.compiled g)
      (Pair.map2 (+) (partialDerivatives f) (VectorSurfaceFunction3D.partialDerivatives g))

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3D space1)
    (Vector3D meters space2)
    (SurfaceFunction3D space1)
  where
  f + v = f + VectorSurfaceFunction3D.constant v

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  f - g =
    new
      (compiled f - VectorSurfaceFunction3D.compiled g)
      (Pair.map2 (-) (partialDerivatives f) (VectorSurfaceFunction3D.partialDerivatives g))

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3D space1)
    (Vector3D meters space2)
    (SurfaceFunction3D space1)
  where
  f - v = f - VectorSurfaceFunction3D.constant v

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
  function - point = function - constant point

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (SurfaceFunction3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  point - function = constant point - function

instance
  Composition
    (SurfaceFunction3D space)
    (Region2D Unitless)
    (Surface3D space)
  where
  function . domain = Surface3D.parametric function domain

instance
  Composition
    (SurfaceFunction3D space)
    (SurfaceFunction2D Unitless)
    (SurfaceFunction3D space)
  where
  f . g = do
    let (dfdx, dfdy) = Pair.map (. g) (partialDerivatives f)
    let (dgdu, dgdv) = SurfaceFunction2D.partialDerivatives g
    let (dxdu, dydu) = VectorSurfaceFunction2D.components dgdu
    let (dxdv, dydv) = VectorSurfaceFunction2D.components dgdv
    let compiledComposed = compiled f . SurfaceFunction2D.compiled g
    let composedPartialDerivatives =
          ( dfdx * dxdu + dfdy * dydu
          , dfdx * dxdv + dfdy * dydv
          )
    new compiledComposed composedPartialDerivatives

new ::
  Compiled space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space) ->
  SurfaceFunction3D space
new givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge
          VectorSurfaceFunction3D.new
          VectorSurfaceFunction3D.compiled
          VectorSurfaceFunction3D.partialDerivatives
          givenPartialDerivatives
  recursive \result ->
    SurfaceFunction3D
      { compiled = givenCompiled
      , partialDerivatives = mergedPartialDerivatives
      , maxSampledNondegeneracy = NonEmpty.maximumOf (nondegeneracy result) UvPoint.interiorSamples
      }

constant :: Point3D space -> SurfaceFunction3D space
constant value =
  new (CompiledFunction.constant value) (VectorSurfaceFunction3D.zero, VectorSurfaceFunction3D.zero)

nondegeneracy :: SurfaceFunction3D space -> UvPoint -> Length
nondegeneracy function uvPoint = do
  let (duValue, dvValue) = partialDerivativesAt uvPoint function
  let duMagnitude = Vector3D.magnitude duValue
  let dvMagnitude = Vector3D.magnitude dvValue
  let minMagnitude = min duMagnitude dvMagnitude
  if minMagnitude == Length.zero
    then Length.zero
    else do
      let crossMagnitude = Vector3D.magnitude (duValue `cross` dvValue)
      let duPerpendicularity = crossMagnitude / dvMagnitude
      let dvPerpendicularity = crossMagnitude / duMagnitude
      let minPerpendicularity = min duPerpendicularity dvPerpendicularity
      min minMagnitude minPerpendicularity

{-# INLINE pointAt #-}
pointAt :: UvPoint -> SurfaceFunction3D space -> Point3D space
pointAt uvPoint function = CompiledFunction.value function.compiled uvPoint

{-# INLINE pointOn #-}
pointOn :: SurfaceFunction3D space -> UvPoint -> Point3D space
pointOn function uvPoint = pointAt uvPoint function

{-# INLINE range #-}
range :: UvBounds -> SurfaceFunction3D space -> Bounds3D space
range uvRange function = CompiledFunction.range function.compiled uvRange

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

{-# INLINE compiled #-}
compiled :: SurfaceFunction3D space -> Compiled space
compiled = (.compiled)

{-# INLINE partialDerivatives #-}
partialDerivatives ::
  SurfaceFunction3D space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space)
partialDerivatives = (.partialDerivatives)

nondegenerate ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  Result IsDegenerate (Nondegenerate (SurfaceFunction3D space))
nondegenerate function =
  if function.maxSampledNondegeneracy ~= Length.zero
    then Err IsDegenerate
    else Ok (Nondegenerate function)

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

transformBy :: Transform3D tag space -> SurfaceFunction3D space -> SurfaceFunction3D space
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
  new compiledTransformed transformedDerivatives

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
  new compiledPlaced placedPartialDerivatives

relativeTo :: Frame3D global local -> SurfaceFunction3D global -> SurfaceFunction3D local
relativeTo frame = placeIn (Frame3D.inverse frame)
