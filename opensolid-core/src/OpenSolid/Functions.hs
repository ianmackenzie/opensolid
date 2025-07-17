module OpenSolid.Functions
  ( Curve (..)
  , CurveCompiled
  , curveNew
  , curveRecursive
  , curveConstant
  , curveZero
  , curveOfCurve
  , curveOfSurfaceFunction
  , curveEvaluate
  , curveEvaluateBounds
  , Curve2d (..)
  , Curve2dCompiled
  , curve2dEvaluate
  , curve2dEvaluateBounds
  , curve2dXCoordinate
  , curve2dYCoordinate
  , curve2dCoordinates
  , VectorCurve2d (..)
  , VectorCurve2dCompiled
  , vectorCurve2dZero
  , vectorCurve2dConstant
  , vectorCurve2dXComponent
  , vectorCurve2dYComponent
  , vectorCurve2dComponents
  , SurfaceFunction (..)
  , SurfaceFunctionCompiled
  , surfaceFunctionNew
  , surfaceFunctionRecursive
  , surfaceFunctionConstant
  , surfaceFunctionZero
  , surfaceFunctionOfCurve
  , surfaceFunctionOfSurfaceFunction
  , surfaceFunctionEvaluate
  , surfaceFunctionEvaluateBounds
  , surfaceFunctionDerivative
  , SurfaceFunction2d (..)
  , SurfaceFunction2dCompiled
  , surfaceFunction2dNew
  , surfaceFunction2dDerivative
  , surfaceFunction2dEvaluate
  , surfaceFunction2dEvaluateBounds
  , surfaceFunction2dXCoordinate
  , surfaceFunction2dYCoordinate
  , surfaceFunction2dCoordinates
  , VectorSurfaceFunction2d (..)
  , VectorSurfaceFunction2dCompiled
  , vectorSurfaceFunction2dNew
  , vectorSurfaceFunction2dConstant
  , vectorSurfaceFunction2dZero
  , vectorSurfaceFunction2dDerivative
  , vectorSurfaceFunction2dXComponent
  , vectorSurfaceFunction2dYComponent
  , vectorSurfaceFunction2dComponents
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d

----- Curve -----

data Curve units where
  Curve ::
    { compiled :: CurveCompiled units
    , derivative :: ~(Curve units)
    , composeCurve :: Curve Unitless -> Curve units
    , composeSurfaceFunction :: SurfaceFunction Unitless -> SurfaceFunction units
    } ->
    Curve units

type CurveCompiled units =
  CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)

instance HasUnits (Curve units) units

instance Units.Coercion (Curve units1) (Curve units2) where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , composeCurve = Units.coerce . curve.composeCurve
      , composeSurfaceFunction = Units.coerce . curve.composeSurfaceFunction
      }

instance Composition (Curve Unitless) (Curve units) (Curve units) where
  f . g = f.composeCurve g

instance Composition (SurfaceFunction Unitless) (Curve units) (SurfaceFunction units) where
  f . g = f.composeSurfaceFunction g

instance FFI (Curve Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Qty units2) units1
  where
  curve ~= value = List.allTrue [curveEvaluate curve tValue ~= value | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Curve units2) units1
  where
  curve1 ~= curve2 =
    List.allTrue
      [ curveEvaluate curve1 tValue ~= curveEvaluate curve2 tValue
      | tValue <- Parameter.samples
      ]

instance Negation (Curve units) where
  negate curve = curveNew do
    #compiled (negate curve.compiled)
    #derivative (negate curve.derivative)
    #composeCurve (\inner -> negate (curve . inner))
    #composeSurfaceFunction (\inner -> negate (curve . inner))

instance Multiplication Sign (Curve units) (Curve units) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (Curve units) Sign (Curve units) where
  curve * Positive = curve
  curve * Negative = -curve

instance units1 ~ units2 => Addition (Curve units1) (Curve units2) (Curve units1) where
  lhs + rhs = curveNew do
    #compiled (lhs.compiled + rhs.compiled)
    #derivative (lhs.derivative + rhs.derivative)
    #composeCurve (\inner -> lhs . inner + rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner + rhs . inner)

instance units1 ~ units2 => Addition (Curve units1) (Qty units2) (Curve units1) where
  curve + value = curve + curveConstant value

instance units1 ~ units2 => Addition (Qty units1) (Curve units2) (Curve units1) where
  value + curve = curveConstant value + curve

instance units1 ~ units2 => Subtraction (Curve units1) (Curve units2) (Curve units1) where
  lhs - rhs = curveNew do
    #compiled (lhs.compiled - rhs.compiled)
    #derivative (lhs.derivative - rhs.derivative)
    #composeCurve (\inner -> lhs . inner - rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner - rhs . inner)

instance
  units1 ~ units2 =>
  Subtraction (Curve units1) (Qty units2) (Curve units1)
  where
  curve - value = curve - curveConstant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Curve units2) (Curve units1)
  where
  value - curve = curveConstant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Curve units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Curve units1) (Curve units2) (Curve (units1 :*: units2)) where
  lhs .*. rhs = curveNew do
    #compiled (lhs.compiled .*. rhs.compiled)
    #derivative (lhs.derivative .*. rhs + lhs .*. rhs.derivative)
    #composeCurve (\inner -> (lhs . inner) .*. (rhs . inner))
    #composeSurfaceFunction (\inner -> (lhs . inner) .*. (rhs . inner))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Qty units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Curve units1) (Qty units2) (Curve (units1 :*: units2)) where
  curve .*. value = curve .*. curveConstant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Curve units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Qty units1) (Curve units2) (Curve (units1 :*: units2)) where
  value .*. curve = curveConstant value .*. curve

curveNew ::
  ( "compiled" ::: CurveCompiled units
  , "derivative" ::: Curve units
  , "composeCurve" ::: (Curve Unitless -> Curve units)
  , "composeSurfaceFunction" ::: (SurfaceFunction Unitless -> SurfaceFunction units)
  ) ->
  Curve units
curveNew args =
  Curve
    { compiled = args.compiled
    , derivative = args.derivative
    , composeCurve = args.composeCurve
    , composeSurfaceFunction = args.composeSurfaceFunction
    }

curveRecursive ::
  ( Curve units ->
    ( "compiled" ::: CurveCompiled units
    , "derivative" ::: Curve units
    , "composeCurve" ::: (Curve Unitless -> Curve units)
    , "composeSurfaceFunction" ::: (SurfaceFunction Unitless -> SurfaceFunction units)
    )
  ) ->
  Curve units
curveRecursive callback = let result = curveNew (callback result) in result

curveConstant :: Qty units -> Curve units
curveConstant value = curveRecursive \self -> do
  #compiled (CompiledFunction.constant value)
  #derivative curveZero
  #composeCurve (always self)
  #composeSurfaceFunction (always (surfaceFunctionConstant value))

curveZero :: Curve units
curveZero = curveConstant Qty.zero

{-# INLINE curveEvaluate #-}
curveEvaluate :: Curve units -> Float -> Qty units
curveEvaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE curveEvaluateBounds #-}
curveEvaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
curveEvaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

curveOfCurve :: Curve Unitless -> Curve units -> Curve units
curveOfCurve inner outer = curveNew do
  let evaluate tValue = curveEvaluate outer (curveEvaluate inner tValue)
  let evaluateBounds tRange = curveEvaluateBounds outer (curveEvaluateBounds inner tRange)
  #compiled (CompiledFunction.abstract evaluate evaluateBounds)
  #derivative ((outer.derivative . inner) * inner.derivative)
  #composeCurve (\newInner -> curveOfCurve (inner . newInner) outer)
  #composeSurfaceFunction (\newInner -> curveOfSurfaceFunction (inner . newInner) outer)

curveOfSurfaceFunction :: SurfaceFunction Unitless -> Curve units -> SurfaceFunction units
curveOfSurfaceFunction inner outer = surfaceFunctionNew do
  let evaluate uvPoint = curveEvaluate outer (surfaceFunctionEvaluate inner uvPoint)
  let evaluateBounds uvBounds =
        curveEvaluateBounds outer (surfaceFunctionEvaluateBounds inner uvBounds)
  #compiled (CompiledFunction.abstract evaluate evaluateBounds)
  let outerDerivative = outer.derivative . inner
  #derivative (\p -> outerDerivative * surfaceFunctionDerivative p inner)
  #composeCurve (\newInner -> curveOfCurve (inner . newInner) outer)
  #composeSurfaceFunction (\newInner -> curveOfSurfaceFunction (inner . newInner) outer)

----- Curve2d -----

-- | A parametric curve in 2D space.
data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d ::
    { compiled :: Curve2dCompiled (space @ units)
    , derivative :: ~(VectorCurve2d (space @ units))
    } ->
    Curve2d (space @ units)

type Curve2dCompiled coordinateSystem =
  CompiledFunction Float (Point2d coordinateSystem) (Bounds Unitless) (Bounds2d coordinateSystem)

instance HasField "secondDerivative" (Curve2d (space @ units)) (VectorCurve2d (space @ units)) where
  getField = (.derivative.derivative)

instance HasField "xCoordinate" (Curve2d (space @ units)) (Curve units) where
  getField = curve2dXCoordinate

instance HasField "yCoordinate" (Curve2d (space @ units)) (Curve units) where
  getField = curve2dYCoordinate

instance HasField "coordinates" (Curve2d (space @ units)) (Curve units, Curve units) where
  getField = curve2dCoordinates

instance Composition (Curve Unitless) (Curve2d (space @ units)) (Curve2d (space @ units)) where
  f . g = Curve2d (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (Curve2d (space @ units))
    (SurfaceFunction2d (space @ units))
  where
  curve . function =
    SurfaceFunction2d
      @ curve.compiled . function.compiled
      @ curve.derivative . function * function.du
      @ curve.derivative . function * function.dv

{-# INLINE curve2dEvaluate #-}
curve2dEvaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
curve2dEvaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE curve2dEvaluateBounds #-}
curve2dEvaluateBounds :: Curve2d (space @ units) -> Bounds Unitless -> Bounds2d (space @ units)
curve2dEvaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

curve2dXCoordinate :: Curve2d (space @ units) -> Curve units
curve2dXCoordinate curve = curveNew do
  #compiled do
    CompiledFunction.map
      Expression.xCoordinate
      Point2d.xCoordinate
      Bounds2d.xCoordinate
      curve.compiled
  #derivative curve.derivative.xComponent
  #composeCurve (\inner -> (curve . inner).xCoordinate)
  #composeSurfaceFunction (\inner -> (curve . inner).xCoordinate)

curve2dYCoordinate :: Curve2d (space @ units) -> Curve units
curve2dYCoordinate curve = curveNew do
  #compiled do
    CompiledFunction.map
      Expression.yCoordinate
      Point2d.yCoordinate
      Bounds2d.yCoordinate
      curve.compiled
  #derivative curve.derivative.yComponent
  #composeCurve (\inner -> (curve . inner).yCoordinate)
  #composeSurfaceFunction (\inner -> (curve . inner).yCoordinate)

curve2dCoordinates :: Curve2d (space @ units) -> (Curve units, Curve units)
curve2dCoordinates curve = (curve.xCoordinate, curve.yCoordinate)

----- VectorCurve2d -----

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    { compiled :: VectorCurve2dCompiled (space @ units)
    , derivative :: ~(VectorCurve2d (space @ units))
    } ->
    VectorCurve2d (space @ units)

type VectorCurve2dCompiled coordinateSystem =
  CompiledFunction
    Float
    (Vector2d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds2d coordinateSystem)

instance HasUnits (VectorCurve2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  coerce curve = VectorCurve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance HasField "xComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField = vectorCurve2dXComponent

instance HasField "yComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField = vectorCurve2dYComponent

instance HasField "components" (VectorCurve2d (space @ units)) (Curve units, Curve units) where
  getField = vectorCurve2dComponents

instance
  Composition
    (Curve Unitless)
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  f . g = VectorCurve2d (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  curve . function = do
    let curveDerivative = curve.derivative . function
    VectorSurfaceFunction2d
      @ curve.compiled . function.compiled
      @ curveDerivative * function.du
      @ curveDerivative * function.dv

instance Negation (VectorCurve2d (space @ units)) where
  negate curve = VectorCurve2d (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  lhs + rhs = VectorCurve2d (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve + vector = curve + vectorCurve2dConstant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  vector + curve = vectorCurve2dConstant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  lhs - rhs = VectorCurve2d (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve - vector = curve - vectorCurve2dConstant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  vector - curve = vectorCurve2dConstant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    VectorCurve2d (lhs.compiled .*. rhs.compiled) (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  c1 .*. c2 = curveConstant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    VectorCurve2d (lhs.compiled .*. rhs.compiled) (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  curve .*. value = curve .*. curveConstant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

vectorCurve2dConstant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
vectorCurve2dConstant value = VectorCurve2d (CompiledFunction.constant value) vectorCurve2dZero

vectorCurve2dZero :: VectorCurve2d (space @ units)
vectorCurve2dZero = vectorCurve2dConstant Vector2d.zero

vectorCurve2dXComponent :: VectorCurve2d (space @ units) -> Curve units
vectorCurve2dXComponent curve = curveNew do
  #compiled do
    CompiledFunction.map
      Expression.xComponent
      Vector2d.xComponent
      VectorBounds2d.xComponent
      curve.compiled
  #derivative curve.derivative.xComponent
  #composeCurve (\inner -> (curve . inner).xComponent)
  #composeSurfaceFunction (\inner -> (curve . inner).xComponent)

vectorCurve2dYComponent :: VectorCurve2d (space @ units) -> Curve units
vectorCurve2dYComponent curve = curveNew do
  #compiled do
    CompiledFunction.map
      Expression.yComponent
      Vector2d.yComponent
      VectorBounds2d.yComponent
      curve.compiled
  #derivative curve.derivative.yComponent
  #composeCurve (\inner -> (curve . inner).yComponent)
  #composeSurfaceFunction (\inner -> (curve . inner).yComponent)

vectorCurve2dComponents :: VectorCurve2d (space @ units) -> (Curve units, Curve units)
vectorCurve2dComponents curve = (curve.xComponent, curve.yComponent)

----- SurfaceFunction -----

data SurfaceFunction units where
  SurfaceFunction ::
    { compiled :: SurfaceFunctionCompiled units
    , du :: ~(SurfaceFunction units)
    , dv :: ~(SurfaceFunction units)
    , composeCurve :: Curve2d UvCoordinates -> Curve units
    , composeSurfaceFunction :: SurfaceFunction2d UvCoordinates -> SurfaceFunction units
    } ->
    SurfaceFunction units

type SurfaceFunctionCompiled units =
  CompiledFunction UvPoint (Qty units) UvBounds (Bounds units)

instance HasUnits (SurfaceFunction units) units

instance Units.Coercion (SurfaceFunction unitsA) (SurfaceFunction unitsB) where
  coerce surfaceFunction =
    SurfaceFunction
      { compiled = Units.coerce surfaceFunction.compiled
      , du = Units.coerce surfaceFunction.du
      , dv = Units.coerce surfaceFunction.dv
      , composeCurve = Units.coerce . surfaceFunction.composeCurve
      , composeSurfaceFunction = Units.coerce . surfaceFunction.composeSurfaceFunction
      }

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction units)
    (Curve units)
  where
  outer . inner = outer.composeCurve inner

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (SurfaceFunction units)
    (SurfaceFunction units)
  where
  outer . inner = outer.composeSurfaceFunction inner

instance Negation (SurfaceFunction units) where
  negate function = surfaceFunctionNew do
    #compiled (negate function.compiled)
    #derivative (\p -> negate (surfaceFunctionDerivative p function))
    #composeCurve (\inner -> negate (function . inner))
    #composeSurfaceFunction (\inner -> negate (function . inner))

instance Multiplication Sign (SurfaceFunction units) (SurfaceFunction units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication (SurfaceFunction units) Sign (SurfaceFunction units) where
  function * Positive = function
  function * Negative = -function

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)
  where
  lhs + rhs = surfaceFunctionNew do
    #compiled (lhs.compiled + rhs.compiled)
    #derivative (\p -> surfaceFunctionDerivative p lhs + surfaceFunctionDerivative p rhs)
    #composeCurve (\inner -> lhs . inner + rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner + rhs . inner)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction units1)
  where
  function + value = function + surfaceFunctionConstant value

instance
  units1 ~ units2 =>
  Addition
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)
  where
  value + function = surfaceFunctionConstant value + function

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  lhs - rhs = surfaceFunctionNew do
    #compiled (lhs.compiled - rhs.compiled)
    #derivative (\p -> surfaceFunctionDerivative p lhs - surfaceFunctionDerivative p rhs)
    #composeCurve (\inner -> lhs . inner - rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner - rhs . inner)

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (Qty units2) (SurfaceFunction units1)
  where
  function - value = function - surfaceFunctionConstant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  value - function = surfaceFunctionConstant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :*: units2))
  where
  lhs .*. rhs = surfaceFunctionNew do
    #compiled (lhs.compiled .*. rhs.compiled)
    #derivative do
      \p -> surfaceFunctionDerivative p lhs .*. rhs + lhs .*. surfaceFunctionDerivative p rhs
    #composeCurve (\inner -> lhs . inner .*. rhs . inner)
    #composeSurfaceFunction (\inner -> lhs . inner .*. rhs . inner)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (Qty units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction (units1 :*: units2))
  where
  function .*. value = function .*. surfaceFunctionConstant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :*: units2))
  where
  value .*. function = surfaceFunctionConstant value .*. function

surfaceFunctionNew ::
  ( "compiled" ::: SurfaceFunctionCompiled units
  , "derivative" ::: (SurfaceParameter -> SurfaceFunction units)
  , "composeCurve" ::: (Curve2d UvCoordinates -> Curve units)
  , "composeSurfaceFunction" ::: (SurfaceFunction2d UvCoordinates -> SurfaceFunction units)
  ) ->
  SurfaceFunction units
surfaceFunctionNew args = do
  let du = args.derivative U
  let dv = args.derivative V
  SurfaceFunction
    { compiled = args.compiled
    , du = du
    , dv =
        SurfaceFunction
          { compiled = dv.compiled
          , dv = dv.dv
          , du = du.dv
          , composeCurve = dv.composeCurve
          , composeSurfaceFunction = dv.composeSurfaceFunction
          }
    , composeCurve = args.composeCurve
    , composeSurfaceFunction = args.composeSurfaceFunction
    }

surfaceFunctionRecursive ::
  ( SurfaceFunction units ->
    ( "compiled" ::: SurfaceFunctionCompiled units
    , "derivative" ::: (SurfaceParameter -> SurfaceFunction units)
    , "composeCurve" ::: (Curve2d UvCoordinates -> Curve units)
    , "composeSurfaceFunction" ::: (SurfaceFunction2d UvCoordinates -> SurfaceFunction units)
    )
  ) ->
  SurfaceFunction units
surfaceFunctionRecursive callback = let result = surfaceFunctionNew (callback result) in result

surfaceFunctionConstant :: Qty units -> SurfaceFunction units
surfaceFunctionConstant value = surfaceFunctionRecursive \self -> do
  #compiled (CompiledFunction.constant value)
  #derivative (always surfaceFunctionZero)
  #composeCurve (always (curveConstant value))
  #composeSurfaceFunction (always self)

surfaceFunctionZero :: SurfaceFunction units
surfaceFunctionZero = surfaceFunctionConstant Qty.zero

surfaceFunctionOfCurve :: Curve2d UvCoordinates -> SurfaceFunction units -> Curve units
surfaceFunctionOfCurve inner outer = curveNew do
  let evaluate tValue = surfaceFunctionEvaluate outer (curve2dEvaluate inner tValue)
  let evaluateBounds tBounds =
        surfaceFunctionEvaluateBounds outer (curve2dEvaluateBounds inner tBounds)
  let (dudt, dvdt) = inner.derivative.components
  #compiled (CompiledFunction.abstract evaluate evaluateBounds)
  #derivative (outer.du . inner * dudt + outer.dv . inner * dvdt)
  #composeCurve (\newInner -> surfaceFunctionOfCurve (inner . newInner) outer)
  #composeSurfaceFunction (\newInner -> surfaceFunctionOfSurfaceFunction (inner . newInner) outer)

surfaceFunctionOfSurfaceFunction ::
  SurfaceFunction2d UvCoordinates ->
  SurfaceFunction units ->
  SurfaceFunction units
surfaceFunctionOfSurfaceFunction inner outer = surfaceFunctionNew do
  let evaluate uvPoint = surfaceFunctionEvaluate outer (surfaceFunction2dEvaluate inner uvPoint)
  let evaluateBounds uvBounds =
        surfaceFunctionEvaluateBounds outer (surfaceFunction2dEvaluateBounds inner uvBounds)
  let dfdu = outer.du . inner
  let dfdv = outer.dv . inner
  #compiled (CompiledFunction.abstract evaluate evaluateBounds)
  #derivative do
    \p -> do
      let (dudp, dvdp) = (surfaceFunction2dDerivative p inner).components
      dfdu * dudp + dfdv * dvdp
  #composeCurve (\newInner -> surfaceFunctionOfCurve (inner . newInner) outer)
  #composeSurfaceFunction (\newInner -> surfaceFunctionOfSurfaceFunction (inner . newInner) outer)

{-# INLINE surfaceFunctionEvaluate #-}
surfaceFunctionEvaluate :: SurfaceFunction units -> UvPoint -> Qty units
surfaceFunctionEvaluate surfaceFunction uvPoint =
  CompiledFunction.evaluate surfaceFunction.compiled uvPoint

{-# INLINE surfaceFunctionEvaluateBounds #-}
surfaceFunctionEvaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
surfaceFunctionEvaluateBounds surfaceFunction uvBounds =
  CompiledFunction.evaluateBounds surfaceFunction.compiled uvBounds

surfaceFunctionDerivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
surfaceFunctionDerivative U function = function.du
surfaceFunctionDerivative V function = function.dv

----- SurfaceFunction2d -----

data SurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction2d ::
    { compiled :: SurfaceFunction2dCompiled (space @ units)
    , du :: ~(VectorSurfaceFunction2d (space @ units))
    , dv :: ~(VectorSurfaceFunction2d (space @ units))
    } ->
    SurfaceFunction2d (space @ units)

type SurfaceFunction2dCompiled coordinateSystem =
  CompiledFunction UvPoint (Point2d coordinateSystem) UvBounds (Bounds2d coordinateSystem)

instance HasField "xCoordinate" (SurfaceFunction2d (space @ units)) (SurfaceFunction units) where
  getField = surfaceFunction2dXCoordinate

instance HasField "yCoordinate" (SurfaceFunction2d (space @ units)) (SurfaceFunction units) where
  getField = surfaceFunction2dYCoordinate

instance
  HasField
    "coordinates"
    (SurfaceFunction2d (space @ units))
    (SurfaceFunction units, SurfaceFunction units)
  where
  getField = surfaceFunction2dCoordinates

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction2d (space @ units))
    (Curve2d (space @ units))
  where
  function . curve = do
    let (dudt, dvdt) = curve.derivative.components
    Curve2d
      @ function.compiled . curve.compiled
      @ (function.du . curve) * dudt + (function.dv . curve) * dvdt

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (SurfaceFunction2d (space @ units))
    (SurfaceFunction2d (space @ units))
  where
  f . g = do
    let dfdu = f.du . g
    let dfdv = f.dv . g
    surfaceFunction2dNew
      @ f.compiled . g.compiled
      @ \p -> do
        let (dudp, dvdp) = (surfaceFunction2dDerivative p g).components
        dfdu * dudp + dfdv * dvdp

surfaceFunction2dNew ::
  SurfaceFunction2dCompiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  SurfaceFunction2d (space @ units)
surfaceFunction2dNew compiled derivative = do
  let du = derivative U
  let dv = derivative V
  let dv' = VectorSurfaceFunction2d dv.compiled du.dv dv.dv
  SurfaceFunction2d compiled du dv'

surfaceFunction2dDerivative ::
  SurfaceParameter ->
  SurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
surfaceFunction2dDerivative U function = function.du
surfaceFunction2dDerivative V function = function.dv

{-# INLINE surfaceFunction2dEvaluate #-}
surfaceFunction2dEvaluate ::
  SurfaceFunction2d (space @ units) ->
  UvPoint ->
  Point2d (space @ units)
surfaceFunction2dEvaluate function uvPoint =
  CompiledFunction.evaluate function.compiled uvPoint

{-# INLINE surfaceFunction2dEvaluateBounds #-}
surfaceFunction2dEvaluateBounds ::
  SurfaceFunction2d (space @ units) ->
  UvBounds ->
  Bounds2d (space @ units)
surfaceFunction2dEvaluateBounds function uvBounds =
  CompiledFunction.evaluateBounds function.compiled uvBounds

surfaceFunction2dXCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
surfaceFunction2dXCoordinate function = surfaceFunctionNew do
  #compiled do
    CompiledFunction.map
      Expression.xCoordinate
      Point2d.xCoordinate
      Bounds2d.xCoordinate
      function.compiled
  #derivative (\parameter -> (surfaceFunction2dDerivative parameter function).xComponent)
  #composeCurve (\inner -> (function . inner).xCoordinate)
  #composeSurfaceFunction (\inner -> (function . inner).xCoordinate)

surfaceFunction2dYCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
surfaceFunction2dYCoordinate function = surfaceFunctionNew do
  #compiled do
    CompiledFunction.map
      Expression.yCoordinate
      Point2d.yCoordinate
      Bounds2d.yCoordinate
      function.compiled
  #derivative (\parameter -> (surfaceFunction2dDerivative parameter function).yComponent)
  #composeCurve (\inner -> (function . inner).yCoordinate)
  #composeSurfaceFunction (\inner -> (function . inner).yCoordinate)

surfaceFunction2dCoordinates ::
  SurfaceFunction2d (space @ units) ->
  (SurfaceFunction units, SurfaceFunction units)
surfaceFunction2dCoordinates function = (function.xCoordinate, function.yCoordinate)

----- VectorSurfaceFunction2d -----

data VectorSurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction2d ::
    { compiled :: VectorSurfaceFunction2dCompiled (space @ units)
    , du :: ~(VectorSurfaceFunction2d (space @ units))
    , dv :: ~(VectorSurfaceFunction2d (space @ units))
    } ->
    VectorSurfaceFunction2d (space @ units)

type VectorSurfaceFunction2dCompiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Vector2d coordinateSystem)
    UvBounds
    (VectorBounds2d coordinateSystem)

instance HasUnits (VectorSurfaceFunction2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction2d (space1 @ unitsA))
    (VectorSurfaceFunction2d (space2 @ unitsB))
  where
  coerce (VectorSurfaceFunction2d c du dv) =
    VectorSurfaceFunction2d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance
  HasField
    "xComponent"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units)
  where
  getField = vectorSurfaceFunction2dXComponent

instance
  HasField
    "yComponent"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units)
  where
  getField = vectorSurfaceFunction2dYComponent

instance
  HasField
    "components"
    (VectorSurfaceFunction2d (space @ units))
    (SurfaceFunction units, SurfaceFunction units)
  where
  getField = vectorSurfaceFunction2dComponents

instance Negation (VectorSurfaceFunction2d (space @ units)) where
  negate function =
    VectorSurfaceFunction2d
      (negate function.compiled)
      (negate function.du)
      (negate function.dv)

instance
  Multiplication
    Sign
    (VectorSurfaceFunction2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication
    (VectorSurfaceFunction2d (space @ units))
    Sign
    (VectorSurfaceFunction2d (space @ units))
  where
  function * Positive = function
  function * Negative = -function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  lhs + rhs =
    vectorSurfaceFunction2dNew
      (lhs.compiled + rhs.compiled)
      (\p -> vectorSurfaceFunction2dDerivative p lhs + vectorSurfaceFunction2dDerivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  f + v = f + vectorSurfaceFunction2dConstant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  v + f = vectorSurfaceFunction2dConstant v + f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  lhs - rhs =
    vectorSurfaceFunction2dNew
      (lhs.compiled - rhs.compiled)
      (\p -> vectorSurfaceFunction2dDerivative p lhs - vectorSurfaceFunction2dDerivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorSurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  f - v = f - vectorSurfaceFunction2dConstant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  v - f = vectorSurfaceFunction2dConstant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    vectorSurfaceFunction2dNew
      @ lhs.compiled .*. rhs.compiled
      @ \p ->
        surfaceFunctionDerivative p lhs .*. rhs + lhs .*. vectorSurfaceFunction2dDerivative p rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  f1 .*. f2 = surfaceFunctionConstant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    vectorSurfaceFunction2dNew
      @ lhs.compiled .*. rhs.compiled
      @ \p ->
        vectorSurfaceFunction2dDerivative p lhs .*. rhs + lhs .*. surfaceFunctionDerivative p rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorSurfaceFunction2d (space @ units1))
    (Qty units2)
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  function .*. value = function .*. surfaceFunctionConstant value

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (VectorSurfaceFunction2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  function . curve = do
    let (dudt, dvdt) = curve.derivative.components
    VectorCurve2d
      @ function.compiled . curve.compiled
      @ (function.du . curve) * dudt + (function.dv . curve) * dvdt

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (VectorSurfaceFunction2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  f . g = do
    let dfdu = f.du . g
    let dfdv = f.dv . g
    vectorSurfaceFunction2dNew
      @ f.compiled . g.compiled
      @ \p -> do
        let (dudp, dvdp) = (surfaceFunction2dDerivative p g).components
        dfdu * dudp + dfdv * dvdp

vectorSurfaceFunction2dNew ::
  VectorSurfaceFunction2dCompiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  VectorSurfaceFunction2d (space @ units)
vectorSurfaceFunction2dNew compiled derivative = do
  let du = derivative U
  let dv = derivative V
  let dv' = VectorSurfaceFunction2d dv.compiled du.dv dv.dv
  VectorSurfaceFunction2d compiled du dv'

vectorSurfaceFunction2dConstant ::
  Vector2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
vectorSurfaceFunction2dConstant value =
  vectorSurfaceFunction2dNew (CompiledFunction.constant value) (always vectorSurfaceFunction2dZero)

vectorSurfaceFunction2dZero :: VectorSurfaceFunction2d (space @ units)
vectorSurfaceFunction2dZero = vectorSurfaceFunction2dConstant Vector2d.zero

vectorSurfaceFunction2dDerivative ::
  SurfaceParameter ->
  VectorSurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
vectorSurfaceFunction2dDerivative U function = function.du
vectorSurfaceFunction2dDerivative V function = function.dv

vectorSurfaceFunction2dXComponent ::
  VectorSurfaceFunction2d (space @ units) ->
  SurfaceFunction units
vectorSurfaceFunction2dXComponent function = surfaceFunctionNew do
  #compiled do
    CompiledFunction.map
      Expression.xComponent
      Vector2d.xComponent
      VectorBounds2d.xComponent
      function.compiled
  #derivative (\parameter -> (vectorSurfaceFunction2dDerivative parameter function).xComponent)
  #composeCurve (\inner -> (function . inner).xComponent)
  #composeSurfaceFunction (\inner -> (function . inner).xComponent)

vectorSurfaceFunction2dYComponent ::
  VectorSurfaceFunction2d (space @ units) ->
  SurfaceFunction units
vectorSurfaceFunction2dYComponent function = surfaceFunctionNew do
  #compiled do
    CompiledFunction.map
      Expression.yComponent
      Vector2d.yComponent
      VectorBounds2d.yComponent
      function.compiled
  #derivative (\parameter -> (vectorSurfaceFunction2dDerivative parameter function).yComponent)
  #composeCurve (\inner -> (function . inner).yComponent)
  #composeSurfaceFunction (\inner -> (function . inner).yComponent)

vectorSurfaceFunction2dComponents ::
  VectorSurfaceFunction2d (space @ units) ->
  (SurfaceFunction units, SurfaceFunction units)
vectorSurfaceFunction2dComponents function = (function.xComponent, function.yComponent)
