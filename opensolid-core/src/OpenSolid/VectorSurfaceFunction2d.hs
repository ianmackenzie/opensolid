-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorSurfaceFunction2d
  ( VectorSurfaceFunction2d
  , Compiled
  , new
  , recursive
  , zero
  , constant
  , xy
  , evaluate
  , evaluateBounds
  , xComponent
  , yComponent
  , components
  , derivative
  , transformBy
  , quotient
  , quotient'
  , squaredMagnitude'
  , squaredMagnitude
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface2d qualified as Expression.VectorSurface2d
import OpenSolid.Functions (VectorSurfaceFunction2d (..), VectorSurfaceFunction2dCompiled)
import OpenSolid.Functions qualified as Functions
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

type Compiled coordinateSystem = VectorSurfaceFunction2dCompiled coordinateSystem

instance
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
  Division
    (VectorSurfaceFunction2d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction2d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction2d (space @ (units1 :/: units2)))
  where
  function ./. value = Units.simplify (function .*. (1.0 ./. value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  lhs `cross'` rhs = SurfaceFunction.new do
    #compiled (lhs.compiled `cross'` rhs.compiled)
    #derivative (\p -> derivative p lhs `cross'` rhs + lhs `cross'` derivative p rhs)
    #composeCurve (\inner -> lhs . inner `cross'` rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner `cross'` rhs . inner)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  function `cross'` vector = function `cross'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  vector `cross'` function = constant vector `cross'` function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2d (space1 @ units))
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `cross` rhs = lhs `cross` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d (space2 @ units))
    (SurfaceFunction units)
  where
  lhs `cross` rhs = Vector2d.unit lhs `cross` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  lhs `dot'` rhs = SurfaceFunction.new do
    #compiled (lhs.compiled `dot'` rhs.compiled)
    #derivative (\p -> derivative p lhs `dot'` rhs + lhs `dot'` derivative p rhs)
    #composeCurve (\inner -> lhs . inner `dot'` rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner `dot'` rhs . inner)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  function `dot'` vector = function `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  vector `dot'` function = constant vector `dot'` function

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2d (space1 @ units))
    (Direction2d space2)
    (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (VectorSurfaceFunction2d (space2 @ units))
    (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector2d.unit lhs `dot` rhs

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  VectorSurfaceFunction2d (space @ units)
new = Functions.vectorSurfaceFunction2dNew

recursive ::
  Compiled (space @ units) ->
  ( VectorSurfaceFunction2d (space @ units) ->
    SurfaceParameter ->
    VectorSurfaceFunction2d (space @ units)
  ) ->
  VectorSurfaceFunction2d (space @ units)
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

zero :: VectorSurfaceFunction2d (space @ units)
zero = Functions.vectorSurfaceFunction2dZero

constant :: Vector2d (space @ units) -> VectorSurfaceFunction2d (space @ units)
constant = Functions.vectorSurfaceFunction2dConstant

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  VectorSurfaceFunction2d (space @ units)
xy x y =
  new
    @ CompiledFunction.map2
      Expression.xy
      Vector2d
      VectorBounds2d
      x.compiled
      y.compiled
    @ \p -> xy (SurfaceFunction.derivative p x) (SurfaceFunction.derivative p y)

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
transformBy transform function =
  new
    @ CompiledFunction.map
      (Expression.VectorSurface2d.transformBy transform)
      (Vector2d.transformBy transform)
      (VectorBounds2d.transformBy transform)
      function.compiled
    @ \p -> transformBy transform (derivative p function)

evaluate :: VectorSurfaceFunction2d (space @ units) -> UvPoint -> Vector2d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction2d (space @ units) ->
  UvBounds ->
  VectorBounds2d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
derivative U = (.du)
derivative V = (.dv)

xComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
xComponent = Functions.vectorSurfaceFunction2dXComponent

yComponent :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction units
yComponent = Functions.vectorSurfaceFunction2dYComponent

components ::
  VectorSurfaceFunction2d (space @ units) ->
  (SurfaceFunction units, SurfaceFunction units)
components = Functions.vectorSurfaceFunction2dComponents

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d (space @ units3)
quotient lhs rhs = Units.specialize (quotient' lhs rhs)

quotient' ::
  Tolerance units2 =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2 ->
  VectorSurfaceFunction2d (space @ (units1 :/: units2))
quotient' lhs rhs =
  recursive
    @ CompiledFunction.map2 Expression.quotient' (./.) (./.) lhs.compiled rhs.compiled
    @ \self p ->
      quotient' (derivative p lhs) rhs
        - self * SurfaceFunction.quotient (SurfaceFunction.derivative p rhs) rhs

squaredMagnitude' :: VectorSurfaceFunction2d (space @ units) -> SurfaceFunction (units :*: units)
squaredMagnitude' function = SurfaceFunction.new do
  #compiled do
    CompiledFunction.map
      Expression.squaredMagnitude'
      Vector2d.squaredMagnitude'
      VectorBounds2d.squaredMagnitude'
      function.compiled
  #derivative (\p -> 2.0 * function `dot'` derivative p function)
  #composeCurve (\inner -> VectorCurve2d.squaredMagnitude' (function . inner))
  #composeSurfaceFunction (\inner -> squaredMagnitude' (function . inner))

squaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorSurfaceFunction2d (space @ units1) ->
  SurfaceFunction units2
squaredMagnitude = Units.specialize . squaredMagnitude'
