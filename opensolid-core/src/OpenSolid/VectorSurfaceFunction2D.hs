{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorSurfaceFunction2D
  ( VectorSurfaceFunction2D
  , Compiled
  , new
  , zero
  , constant
  , xy
  , valueAt
  , valueOf
  , range
  , xComponent
  , yComponent
  , components
  , compiled
  , partialDerivatives
  , placeIn
  , relativeTo
  , transformBy
  , squaredMagnitude_
  , squaredMagnitude
  , newtonRaphson
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Pair qualified as Pair
import OpenSolid.PartialDerivatives qualified as PartialDerivatives
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorTransform2D (VectorTransform2D)

data VectorSurfaceFunction2D units = VectorSurfaceFunction2D
  { compiled :: Compiled units
  , partialDerivatives :: (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units)
  }

type Compiled units =
  CompiledFunction
    UvPoint
    (Vector2D units)
    UvBounds
    (VectorBounds2D units)

instance Units (VectorSurfaceFunction2D units) units

instance Units.Coercion (VectorSurfaceFunction2D units1) (VectorSurfaceFunction2D units2) where
  coerce function =
    VectorSurfaceFunction2D
      { compiled = Units.coerce function.compiled
      , partialDerivatives = Pair.map Units.coerce function.partialDerivatives
      }

instance Negation (VectorSurfaceFunction2D units) where
  negate function = new (negate function.compiled) (Pair.map negate function.partialDerivatives)

instance
  Multiplication
    Sign
    (VectorSurfaceFunction2D units)
    (VectorSurfaceFunction2D units)
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction2D units)
    Sign
    (VectorSurfaceFunction2D units)
  where
  function * Positive = function
  function * Negative = -function

instance
  units1 ~ units2 =>
  Addition
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)
  where
  f + g =
    new
      (compiled f + compiled g)
      (Pair.map2 (+) (partialDerivatives f) (partialDerivatives g))

instance
  units1 ~ units2 =>
  Addition
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (VectorSurfaceFunction2D units1)
  where
  f + v = f + constant v

instance
  units1 ~ units2 =>
  Addition
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)
  where
  v + f = constant v + f

instance
  units1 ~ units2 =>
  Subtraction
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)
  where
  f - g =
    new
      (compiled f - compiled g)
      (Pair.map2 (-) (partialDerivatives f) (partialDerivatives g))

instance
  units1 ~ units2 =>
  Subtraction
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (VectorSurfaceFunction2D units1)
  where
  f - v = f - constant v

instance
  units1 ~ units2 =>
  Subtraction
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
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
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorSurfaceFunction2D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
  where
  f1 ?*? f2 = SurfaceFunction1D.constant f1 ?*? f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
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
    (VectorSurfaceFunction2D units1)
    (Quantity units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorSurfaceFunction2D units1)
    (Quantity units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
  where
  function ?*? quantity = function ?*? SurfaceFunction1D.constant quantity

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorSurfaceFunction2D units1)
    (Quantity units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorSurfaceFunction2D units1)
    (Quantity units2)
    (VectorSurfaceFunction2D (units1 ?/? units2))
  where
  function ?/? quantity = Units.simplify (function ?*? (1.0 ?/? quantity))

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  f `cross_` g = do
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let compiledCrossProduct = compiled f `cross_` compiled g
    let crossProductPartialDerivatives =
          ( dfdu `cross_` g + f `cross_` dgdu
          , dfdv `cross_` g + f `cross_` dgdv
          )
    SurfaceFunction1D.new compiledCrossProduct crossProductPartialDerivatives

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function `cross_` vector = function `cross_` constant vector

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  vector `cross_` function = constant vector `cross_` function

instance
  CrossMultiplication
    (VectorSurfaceFunction2D units)
    Direction2D
    (SurfaceFunction1D units)
  where
  lhs `cross` rhs = lhs `cross` Vector2D.unit rhs

instance
  CrossMultiplication
    Direction2D
    (VectorSurfaceFunction2D units)
    (SurfaceFunction1D units)
  where
  lhs `cross` rhs = Vector2D.unit lhs `cross` rhs

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (VectorSurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
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
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (VectorSurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function `dot_` vector = function `dot_` constant vector

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (Vector2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  vector `dot_` function = constant vector `dot_` function

instance
  DotMultiplication
    (VectorSurfaceFunction2D units)
    Direction2D
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector2D.unit rhs

instance
  DotMultiplication
    Direction2D
    (VectorSurfaceFunction2D units)
    (SurfaceFunction1D units)
  where
  lhs `dot` rhs = Vector2D.unit lhs `dot` rhs

instance
  Composition
    (VectorSurfaceFunction2D units)
    (Curve2D Unitless)
    (VectorCurve2D units)
  where
  f . g = do
    let (dfdu, dfdv) = Pair.map (. g) (partialDerivatives f)
    let (dudt, dvdt) = VectorCurve2D.components (Curve2D.derivative g)
    let compiledComposed = compiled f . Curve2D.compiled g
    let composedDerivative = dfdu * dudt + dfdv * dvdt
    VectorCurve2D.new compiledComposed composedDerivative

new ::
  Compiled units ->
  (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units) ->
  VectorSurfaceFunction2D units
new givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge new compiled partialDerivatives givenPartialDerivatives
  VectorSurfaceFunction2D givenCompiled mergedPartialDerivatives

zero :: VectorSurfaceFunction2D units
zero = constant Vector2D.zero

constant :: Vector2D units -> VectorSurfaceFunction2D units
constant vector = new (CompiledFunction.constant vector) (zero, zero)

xy ::
  SurfaceFunction1D units ->
  SurfaceFunction1D units ->
  VectorSurfaceFunction2D units
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Vector2D
          VectorBounds2D
          (SurfaceFunction1D.compiled x)
          (SurfaceFunction1D.compiled y)
  let xyPartialDerivatives =
        Pair.map2
          xy
          (SurfaceFunction1D.partialDerivatives x)
          (SurfaceFunction1D.partialDerivatives y)
  new compiledXY xyPartialDerivatives

placeIn :: Frame2D frameUnits -> VectorSurfaceFunction2D units -> VectorSurfaceFunction2D units
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector2D.placeIn frame)
          (VectorBounds2D.placeIn frame)
          function.compiled
  let placedPartialDerivatives = Pair.map (placeIn frame) (partialDerivatives function)
  new compiledPlaced placedPartialDerivatives

relativeTo :: Frame2D frameUnits -> VectorSurfaceFunction2D units -> VectorSurfaceFunction2D units
relativeTo frame = placeIn (Frame2D.inverse frame)

transformBy ::
  VectorTransform2D tag ->
  VectorSurfaceFunction2D units ->
  VectorSurfaceFunction2D units
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector2D.transformBy transform)
          (VectorBounds2D.transformBy transform)
          function.compiled
  let transformedPartialDerivatives = Pair.map (transformBy transform) (partialDerivatives function)
  new compiledTransformed transformedPartialDerivatives

{-# INLINE valueAt #-}
valueAt :: UvPoint -> VectorSurfaceFunction2D units -> Vector2D units
valueAt uvPoint function = CompiledFunction.value uvPoint function.compiled

{-# INLINE valueOf #-}
valueOf :: VectorSurfaceFunction2D units -> UvPoint -> Vector2D units
valueOf function uvPoint = valueAt uvPoint function

{-# INLINE range #-}
range :: VectorSurfaceFunction2D units -> UvBounds -> VectorBounds2D units
range function uvRange = CompiledFunction.range uvRange function.compiled

{-# INLINE compiled #-}
compiled :: VectorSurfaceFunction2D units -> Compiled units
compiled = (.compiled)

{-# INLINE partialDerivatives #-}
partialDerivatives :: VectorSurfaceFunction2D units -> (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units)
partialDerivatives = (.partialDerivatives)

xComponent :: VectorSurfaceFunction2D units -> SurfaceFunction1D units
xComponent function = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2D.xComponent
          VectorBounds2D.xComponent
          function.compiled
  SurfaceFunction1D.new compiledXComponent (Pair.map xComponent (partialDerivatives function))

yComponent :: VectorSurfaceFunction2D units -> SurfaceFunction1D units
yComponent function = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2D.yComponent
          VectorBounds2D.yComponent
          function.compiled
  SurfaceFunction1D.new compiledYComponent (Pair.map yComponent (partialDerivatives function))

components ::
  VectorSurfaceFunction2D units ->
  (SurfaceFunction1D units, SurfaceFunction1D units)
components function = (xComponent function, yComponent function)

squaredMagnitude_ :: VectorSurfaceFunction2D units -> SurfaceFunction1D (units ?*? units)
squaredMagnitude_ function = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector2D.squaredMagnitude_
          VectorBounds2D.squaredMagnitude_
          function.compiled
  let squaredMagnitudePartialDerivatives =
        Pair.map (2.0 * function `dot_`) function.partialDerivatives
  SurfaceFunction1D.new compiledSquaredMagnitude squaredMagnitudePartialDerivatives

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction2D units1 ->
  SurfaceFunction1D units2
squaredMagnitude = Units.specialize . squaredMagnitude_

newtonRaphson :: VectorSurfaceFunction2D units -> UvPoint -> Fuzzy UvPoint
newtonRaphson f uvPoint0 = do
  let (fu, fv) = partialDerivatives f
  let evaluate uvPoint = (# valueAt uvPoint f, valueAt uvPoint fu, valueAt uvPoint fv #)
  NewtonRaphson.Surface.solveFrom uvPoint0 evaluate
