{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Curve
  ( Curve
  , Exists
  , Segment
  , SearchTree
  , HasSingularity (HasSingularity)
  , new
  , derivative
  , compiled
  , overallBounds
  , point
  , bounds
  , startPoint
  , endPoint
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , tangentDirectionBounds
  , isPoint
  , singular0
  , singular1
  , nondegenerate
  , nonzero
  , tangentDirection
  , curvatureVector_
  , findPoint
  , searchTree
  , Intersections (IntersectionPoints, OverlappingSegments)
  , IntersectionPoint
  , intersections
  , linearDeviation
  , linearize
  , toPolyline
  , arcLengthParameterizationFunction
  , arcLengthParameterization
  , parameterizeByArcLength
  )
where

import OpenSolid.ArcLength qualified as ArcLength
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import {-# SOURCE #-} OpenSolid.Curve.Intersections (Intersections)
import {-# SOURCE #-} OpenSolid.Curve.Intersections qualified as Intersections
import {-# SOURCE #-} OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import {-# SOURCE #-} OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve.Search qualified as Curve.Search
import OpenSolid.Curve.Segment (Segment)
import OpenSolid.Curve.Segment qualified as Curve.Segment
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
-- TODO remove once all typeclass instances are moved here
import {-# SOURCE #-} OpenSolid.Curve2D ()
-- TODO remove once all typeclass instances are moved here
import {-# SOURCE #-} OpenSolid.Curve3D ()
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Expression qualified as Expression
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate), Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Polyline (Polyline (Polyline))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Search qualified as Search
import OpenSolid.Search.Domain qualified as Search.Domain
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

data Curve dimension units space = Curve
  { compiled :: Compiled dimension units space
  , derivative :: ~(VectorCurve dimension units space)
  , startPoint :: ~(Point dimension units space)
  , endPoint :: ~(Point dimension units space)
  , searchTree :: ~(Curve.Search.Tree dimension units space)
  }

type Compiled dimension units space =
  CompiledFunction
    Number
    (Point dimension units space)
    (Interval Unitless)
    (Bounds dimension units space)

data HasSingularity = HasSingularity deriving (Eq, Show)

type SearchTree dimension units space =
  Curve.Search.Tree dimension units space

instance
  space1 ~ space2 =>
  Units.Coercion (Curve 2 units1 space1) (Curve 2 units2 space2)
  where
  coerce curve =
    Curve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startPoint = Units.coerce curve.startPoint
      , endPoint = Units.coerce curve.endPoint
      , searchTree = Units.coerce curve.searchTree
      }

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Curve 2 units1 space1) (Point2D units2 space2) (Tolerance units1)
  where
  curve `intersects` givenPoint = not (List.isEmpty (findPoint givenPoint curve))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2D units1 space1) (Curve 2 units2 space2) (Tolerance units1)
  where
  givenPoint `intersects` curve = curve `intersects` givenPoint

instance
  space1 ~ space2 =>
  Intersects (Curve 3 Meters space1) (Point3D space2) (Tolerance Meters)
  where
  curve `intersects` givenPoint = not (List.isEmpty (findPoint givenPoint curve))

instance
  space1 ~ space2 =>
  Intersects (Point3D space1) (Curve 3 Meters space2) (Tolerance Meters)
  where
  givenPoint `intersects` curve = curve `intersects` givenPoint

instance
  Exists dimension units space =>
  Composition (Curve dimension units space) (Curve1D Unitless) (Curve dimension units space)
  where
  f . g = new (f.compiled . Curve1D.compiled g) ((f.derivative . g) * Curve1D.derivative g)

class
  ( Point.Exists dimension units space
  , Bounds.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , DirectionBounds.Exists dimension space
  , Expression.Evaluation
      Number
      (Point dimension units space)
      (Interval Unitless)
      (Bounds dimension units space)
  , VectorCurve.Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , DirectionCurve.Exists dimension space
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  ) =>
  Exists dimension units space

instance Exists 2 units space

instance Exists 3 Meters space

new ::
  Exists dimension units space =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  Curve dimension units space
new givenCompiled givenDerivative =
  recursive \self ->
    Curve
      { compiled = givenCompiled
      , derivative = givenDerivative
      , startPoint = CompiledFunction.value givenCompiled 0.0
      , endPoint = CompiledFunction.value givenCompiled 1.0
      , searchTree = Curve.Search.tree self
      }

derivative :: Curve dimension units space -> VectorCurve dimension units space
derivative = (.derivative)

compiled :: Curve dimension units space -> Compiled dimension units space
compiled = (.compiled)

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

isPoint :: (Exists dimension units space, Tolerance units) => Curve dimension units space -> Bool
isPoint curve = VectorCurve.isZero (derivative curve)

point :: Curve dimension units space -> Number -> Point dimension units space
point curve 0.0 = curve.startPoint
point curve 1.0 = curve.endPoint
point curve tValue = CompiledFunction.value curve.compiled tValue

startPoint :: Curve dimension units space -> Point dimension units space
startPoint = (.startPoint)

endPoint :: Curve dimension units space -> Point dimension units space
endPoint = (.endPoint)

bounds :: Curve dimension units space -> Interval Unitless -> Bounds dimension units space
bounds curve tBounds = CompiledFunction.bounds curve.compiled tBounds

overallBounds :: Curve dimension units space -> Bounds dimension units space
overallBounds curve = bounds curve Interval.unit

searchTree :: Curve dimension units space -> Curve.Search.Tree dimension units space
searchTree = (.searchTree)

singular0 :: Exists dimension units space => Curve dimension units space -> Bool
singular0 curve = VectorCurve.singular0 (derivative curve)

singular1 :: Exists dimension units space => Curve dimension units space -> Bool
singular1 curve = VectorCurve.singular1 (derivative curve)

nondegenerate ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsDegenerate (Nondegenerate (Curve dimension units space))
nondegenerate curve =
  if VectorCurve.isZero (derivative curve) then Error IsDegenerate else Ok (Nondegenerate curve)

nonzero ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result HasSingularity (Nonzero (Curve dimension units space))
nonzero curve =
  if derivativeValue curve 0.0 ~= Vector.zero || derivativeValue curve 1.0 ~= Vector.zero
    then Error HasSingularity
    else Ok (Nonzero curve)

tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
tangentDirection curve = VectorCurve.direction (derivative curve)

curvatureVector_ ::
  ( Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result HasSingularity (VectorCurve dimension (Unitless ?/? units) space)
curvatureVector_ curve = Result.map Curve.Nonzero.curvatureVector_ (nonzero curve)

derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = VectorCurve.value (derivative curve) tValue

derivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeBounds curve tBounds = VectorCurve.bounds (derivative curve) tBounds

secondDerivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = VectorCurve.value (secondDerivative curve) tValue

secondDerivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeBounds curve tBounds = VectorCurve.bounds (secondDerivative curve) tBounds

tangentDirectionBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
tangentDirectionBounds curve tBounds = VectorCurve.directionBounds (derivative curve) tBounds

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  List Number
findPoint givenPoint curve = do
  let evaluate tValue = (# point curve tValue - givenPoint, derivativeValue curve tValue #)
  let isSolution tValue = point curve tValue ~= givenPoint
  let isDegenerate tValue = derivativeValue curve tValue ~= Vector.zero
  let endpointSolutions = List.filter isSolution [0.0, 1.0]
  let solveMonotonic tBounds = do
        let tMid = Interval.midpoint tBounds
        let tSolution = NewtonRaphson.curve evaluate tMid
        if Search.isInterior tSolution tBounds && isSolution tSolution
          then Resolved (Just tSolution)
          else Unresolved
  let interiorSolution tBounds segment
        | not (givenPoint `intersects` Curve.Segment.bounds segment) = Resolved Nothing
        | otherwise = do
            let isMonotonic = Curve.Segment.monotonic segment
            let isSmall = Search.Domain.isSmall tBounds
            let endpointSolution = List.find (Number.includedIn tBounds) endpointSolutions
            let hasEndpointSolution = endpointSolution /= Nothing
            if
              | isMonotonic && hasEndpointSolution -> Resolved Nothing
              | isSmall, Just tValue <- endpointSolution, isDegenerate tValue -> Resolved Nothing
              | isMonotonic -> solveMonotonic tBounds
              | otherwise -> Unresolved
  let isDuplicate (tBounds1, _) (tBounds2, _) = Search.Domain.overlapping tBounds1 tBounds2
  let interiorSolutions =
        Search.exclusive interiorSolution isDuplicate (searchTree curve)
          & List.map Pair.second
  List.sort (endpointSolutions <> interiorSolutions)

intersections ::
  ( Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe Intersections)
intersections = Intersections.intersections

linearDeviation ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  Quantity units
linearDeviation curve (Interval t1 t2) = do
  let p1 = point curve t1
  let p2 = point curve t2
  let pMid = point curve (Number.midpoint t1 t2)
  let midError = Line.distanceTo pMid (Line p1 p2)
  max midError (leftRightError curve t1 t2 p1 p2)

toPolyline ::
  Exists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  Polyline dimension units space
toPolyline resolution curve =
  Polyline (NonEmpty.map (point curve) (linearize resolution curve))

linearize ::
  Exists dimension units space =>
  Resolution units ->
  Curve dimension units space ->
  NonEmpty Number
linearize resolution curve = do
  let collect (Interval t1 t2) p1 p2 accumulated = do
        let tMid = Number.midpoint t1 t2
        let pMid = point curve tMid
        let midError = Line.distanceTo pMid (Line p1 p2)
        let error = max midError (leftRightError curve t1 t2 p1 p2)
        let size = Point.distanceFrom p1 p2
        if Resolution.acceptable ("size" ::: size) ("error" ::: error) resolution
          then NonEmpty.push t1 accumulated
          else
            accumulated
              & collect (Interval tMid t2) pMid p2
              & collect (Interval t1 tMid) p1 pMid
  collect Interval.unit (startPoint curve) (endPoint curve) (NonEmpty.one 1.0)

leftRightError ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Number ->
  Point dimension units space ->
  Point dimension units space ->
  Quantity units
leftRightError curve t1 t2 p1 p2 = do
  let tWidth = t2 - t1
  let tMid = t1 + 0.5 * tWidth
  let tOffset = 0.5 * tWidth * Number.sqrt (3 / 7)
  let tLeft = tMid + tOffset
  let tRight = tMid - tOffset
  let leftError = Line.distanceTo (point curve tLeft) (Line p1 p2)
  let rightError = Line.distanceTo (point curve tRight) (Line p1 p2)
  max leftError rightError

arcLengthParameterizationFunction ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Number -> Number, Quantity units)
arcLengthParameterizationFunction curve =
  case nondegenerate curve of
    Error IsDegenerate -> (id, Quantity.zero)
    Ok nondegenerateCurve -> do
      let dsdt t = Vector.magnitude (derivativeValue curve t)
      let d2sdt2 t =
            secondDerivativeValue curve t
              `dot` Curve.Nondegenerate.tangentDirectionValue nondegenerateCurve t
      ArcLength.parameterization dsdt d2sdt2

arcLengthParameterization ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Curve1D Unitless, Quantity units)
arcLengthParameterization curve =
  case nondegenerate curve of
    Error IsDegenerate -> (Curve1D.t, Quantity.zero)
    Ok nondegenerateCurve -> do
      let (parameterizationFunction, length) = arcLengthParameterizationFunction curve
      let parameterizationBounds (Interval uLow uHigh) =
            Interval (parameterizationFunction uLow) (parameterizationFunction uHigh)
      let compiledParameterization =
            CompiledFunction.abstract parameterizationFunction parameterizationBounds
      let nondegenerateDerivative = Curve.Nondegenerate.derivative nondegenerateCurve
      let nondegenerateDerivativeMagnitude = VectorCurve.Nondegenerate.magnitude nondegenerateDerivative
      let parameterizationCurve = recursive \self -> do
            let dtdu = Curve1D.constant length / nondegenerateDerivativeMagnitude
            Curve1D.new compiledParameterization (dtdu . self)
      (parameterizationCurve, length)

parameterizeByArcLength ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  (Curve dimension units space, Quantity units)
parameterizeByArcLength curve = do
  let (parameterization, length) = arcLengthParameterization curve
  (curve . parameterization, length)
