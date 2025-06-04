module OpenSolid.VectorSurfaceFunction3d
  ( VectorSurfaceFunction3d
  , Compiled
  , new
  , recursive
  , zero
  , constant
  , rightwardForwardUpward
  , evaluate
  , evaluateBounds
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface3d qualified as Expression.VectorSurface3d
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

data VectorSurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction3d ::
    Compiled (space @ units) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    VectorSurfaceFunction3d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    UvPoint
    (Vector3d coordinateSystem)
    UvBounds
    (VectorBounds3d coordinateSystem)

instance
  HasUnits
    (VectorSurfaceFunction3d (space @ units))
    units
    (VectorSurfaceFunction3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d (space1 @ unitsA))
    (VectorSurfaceFunction3d (space2 @ unitsB))
  where
  coerce (VectorSurfaceFunction3d c du dv) =
    VectorSurfaceFunction3d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance Negation (VectorSurfaceFunction3d (space @ units)) where
  negate function = new (negate function.compiled) (\p -> negate (derivative p function))

instance
  Multiplication
    Sign
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction3d (space @ units))
    Sign
    (VectorSurfaceFunction3d (space @ units))
  where
  function * Positive = function
  function * Negative = -function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (\p -> derivative p lhs + derivative p rhs)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorSurfaceFunction3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  f + v = f + constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  v + f = constant v + f

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (\p -> derivative p lhs - derivative p rhs)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorSurfaceFunction3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  f - v = f - constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (VectorSurfaceFunction3d (space_ @ units_))
    (VectorSurfaceFunction3d (space @ units))
  where
  v - f = constant v - f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  point + function = SurfaceFunction3d.constant point + function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new
      # lhs.compiled .*. rhs.compiled
      # \p -> SurfaceFunction.derivative p lhs .*. rhs + lhs .*. derivative p rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorSurfaceFunction3d (space @ units2)) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  f1 .*. f2 = SurfaceFunction.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorSurfaceFunction3d (space @ units1)) (SurfaceFunction units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new
      # lhs.compiled .*. rhs.compiled
      # \p -> derivative p lhs .*. rhs + lhs .*. SurfaceFunction.derivative p rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorSurfaceFunction3d (space @ units1)) (Qty units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  function .*. value = function .*. SurfaceFunction.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorSurfaceFunction3d (space @ units1)) (SurfaceFunction units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
  where
  lhs ./. rhs =
    recursive
      # lhs.compiled ./. rhs.compiled
      # \self p -> derivative p lhs ./. rhs - self * (SurfaceFunction.derivative p rhs / rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorSurfaceFunction3d (space @ units1)) (Qty units2) (VectorSurfaceFunction3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorSurfaceFunction3d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction3d (space @ (units1 :/: units2)))
  where
  function ./. value = function ./. SurfaceFunction.constant value

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  lhs `cross'` rhs =
    new
      (lhs.compiled `cross'` rhs.compiled)
      (\p -> derivative p lhs `cross'` rhs + lhs `cross'` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  f `cross'` v = f `cross'` constant v

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  v `cross'` f = constant v `cross'` f

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  lhs `dot'` rhs =
    SurfaceFunction.new
      (lhs.compiled `dot'` rhs.compiled)
      (\p -> derivative p lhs `dot'` rhs + lhs `dot'` derivative p rhs)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (Vector3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  function `dot'` vector = function `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector3d (space @ units1))
    (VectorSurfaceFunction3d (space_ @ units2))
    (SurfaceFunction (units1 :*: units2))
  where
  vector `dot'` function = constant vector `dot'` function

instance
  space ~ space_ =>
  DotMultiplication (VectorSurfaceFunction3d (space @ units)) (Direction3d space_) (SurfaceFunction units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (VectorSurfaceFunction3d (space_ @ units)) (SurfaceFunction units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (VectorSurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  outer . inner = do
    let duOuter = derivative U outer . inner
    let dvOuter = derivative V outer . inner
    new
      # outer.compiled . inner.compiled
      # \p -> do
        let dInner = SurfaceFunction2d.derivative p inner
        let dU = dInner.xComponent
        let dV = dInner.yComponent
        duOuter * dU + dvOuter * dV

instance
  HasField
    "compiled"
    (VectorSurfaceFunction3d (space @ units))
    (Compiled (space @ units))
  where
  getField (VectorSurfaceFunction3d c _ _) = c

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction3d (space @ units)) ->
  VectorSurfaceFunction3d (space @ units)
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3d dv.compiled (derivative V du) (derivative V dv)
  VectorSurfaceFunction3d c du dv'

recursive ::
  Compiled (space @ units) ->
  ( VectorSurfaceFunction3d (space @ units) ->
    SurfaceParameter ->
    VectorSurfaceFunction3d (space @ units)
  ) ->
  VectorSurfaceFunction3d (space @ units)
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

zero :: VectorSurfaceFunction3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorSurfaceFunction3d (space @ units)
constant value = new (CompiledFunction.constant value) (always zero)

rightwardForwardUpward ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  VectorSurfaceFunction3d (space @ units)
rightwardForwardUpward r f u =
  new
    # CompiledFunction.map3
      Expression.rightwardForwardUpward
      Vector3d.rightwardForwardUpward
      VectorBounds3d.rightwardForwardUpward
      r.compiled
      f.compiled
      u.compiled
    # \p ->
      rightwardForwardUpward
        (SurfaceFunction.derivative p r)
        (SurfaceFunction.derivative p f)
        (SurfaceFunction.derivative p u)

evaluate :: VectorSurfaceFunction3d (space @ units) -> UvPoint -> Vector3d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds ::
  VectorSurfaceFunction3d (space @ units) ->
  UvBounds ->
  VectorBounds3d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
derivative U (VectorSurfaceFunction3d _ du _) = du
derivative V (VectorSurfaceFunction3d _ _ dv) = dv

placeIn ::
  Orientation3d global (Defines local) ->
  VectorSurfaceFunction3d (local @ units) ->
  VectorSurfaceFunction3d (global @ units)
placeIn orientation function =
  new
    # CompiledFunction.map
      (Expression.VectorSurface3d.placeIn orientation)
      (Vector3d.placeIn orientation)
      (VectorBounds3d.placeIn orientation)
      function.compiled
    # \p -> placeIn orientation (derivative p function)

relativeTo ::
  Orientation3d global (Defines local) ->
  VectorSurfaceFunction3d (global @ units) ->
  VectorSurfaceFunction3d (local @ units)
relativeTo orientation function = placeIn (Orientation3d.inverse orientation) function

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
transformBy transform function =
  new
    # CompiledFunction.map
      (Expression.VectorSurface3d.transformBy transform)
      (Vector3d.transformBy transform)
      (VectorBounds3d.transformBy transform)
      function.compiled
    # \p -> transformBy transform (derivative p function)
