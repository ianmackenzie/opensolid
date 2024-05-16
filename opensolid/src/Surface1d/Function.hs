{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Function
  ( Function
  , Interface (..)
  , evaluateAt
  , pointOn
  , segmentBounds
  , boundsOn
  , derivative
  , derivativeIn
  , zero
  , constant
  , parameter
  , zeros
  , wrap
  , squared
  , sqrt
  , sin
  , cos
  , curveOnSurface
  , isZero
  )
where

import Angle qualified
import Axis2d qualified
import BezierCurve2d qualified
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Int qualified
import Line2d qualified
import List qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Result qualified
import Surface1d.Function.Boundary (Boundary)
import Surface1d.Function.Boundary qualified as Boundary
import Surface1d.Function.PartialZeros (PartialZeros (PartialZeros))
import Surface1d.Function.PartialZeros qualified as PartialZeros
import Surface1d.Function.SaddleRegion (SaddleRegion (SaddleRegion))
import Surface1d.Function.SaddleRegion qualified as SaddleRegion
import Surface1d.Function.Zeros (Zeros (Zeros))
import Surface1d.Function.Zeros qualified as Zeros
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorCurve2d qualified

class Show function => Interface function units | function -> units where
  evaluateAtImpl :: Uv.Point -> function -> Qty units
  segmentBoundsImpl :: Uv.Bounds -> function -> Range units
  derivativeImpl :: Parameter -> function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Zero ::
    Function units
  Constant ::
    Qty units ->
    Function units
  Parameter ::
    Parameter ->
    Function Unitless
  Negated ::
    Function units ->
    Function units
  Sum ::
    Function units ->
    Function units ->
    Function units
  Difference ::
    Function units ->
    Function units ->
    Function units
  Product' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :*: units2)
  Quotient' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :/: units2)
  Squared' ::
    Function units ->
    Function (units :*: units)
  SquareRoot ::
    Units.Squared units1 units2 =>
    Function units2 ->
    Function units1
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless
  Coerce ::
    Function units1 ->
    Function units2

deriving instance Show (Function units)

instance HasUnits (Function units) where
  type Units (Function units) = units
  type Erase (Function units) = Function Unitless

instance Units.Coercion (Function units1) (Function units2) where
  coerce Zero = Zero
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Negation (Function units) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Coerce function) = Coerce (negate function)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units)

instance Multiplication' Sign (Function units) where
  type Sign .*. Function units = Function (Unitless :*: units)
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function units) Sign (Function units)

instance Multiplication' (Function units) Sign where
  type Function units .*. Sign = Function (units :*: Unitless)
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance units ~ units_ => Addition (Function units) (Function units_) (Function units) where
  Zero + function = function
  function + Zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units_ => Addition (Function units) (Qty units_) (Function units) where
  function + value = function + constant value

instance units ~ units_ => Addition (Qty units) (Function units_) (Function units) where
  value + function = constant value + function

instance units ~ units_ => Subtraction (Function units) (Function units_) (Function units) where
  Zero - function = negate function
  function - Zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance units ~ units_ => Subtraction (Function units) (Qty units_) (Function units) where
  function - value = function - constant value

instance units ~ units_ => Subtraction (Qty units) (Function units_) (Function units) where
  value - function = constant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Function units2) (Function units3)

instance Multiplication' (Function units1) (Function units2) where
  type Function units1 .*. Function units2 = Function (units1 :*: units2)
  Zero .*. _ = Zero
  _ .*. Zero = Zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant (Qty 1.0) .*. function = Units.coerce function
  Constant (Qty -1.0) .*. function = Units.coerce (negate function)
  Constant x .*. Negated c = negate x .*. c
  f1 .*. (Constant x) = Units.commute (Constant x .*. f1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  function1 .*. function2 = Product' function1 function2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Qty units2) (Function units3)

instance Multiplication' (Function units1) (Qty units2) where
  type Function units1 .*. Qty units2 = Function (units1 :*: units2)
  function .*. value = function .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function units2) (Function units3)

instance Multiplication' (Qty units1) (Function units2) where
  type Qty units1 .*. Function units2 = Function (units1 :*: units2)
  value .*. function = constant value .*. function

instance Multiplication (Function units) Int (Function units)

instance Multiplication' (Function units) Int where
  type Function units .*. Int = Function (units :*: Unitless)
  function .*. value = function .*. Float.fromInt value

instance Multiplication Int (Function units) (Function units)

instance Multiplication' Int (Function units) where
  type Int .*. Function units = Function (Unitless :*: units)
  value .*. function = Float.fromInt value .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Division' (Function units1) (Function units2) where
  type Function units1 ./. Function units2 = Function (units1 :/: units2)
  Zero ./. _ = Zero
  Constant x ./. Constant y = Constant (x ./. y)
  function ./. Constant x = (1 ./. x) .*^ function
  function1 ./. function2 = Quotient' function1 function2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Qty units2) (Function units3)

instance Division' (Function units1) (Qty units2) where
  type Function units1 ./. Qty units2 = Function (units1 :/: units2)
  function ./. value = function ./. constant value

instance Division (Function units) Int (Function units)

instance Division' (Function units) Int where
  type Function units ./. Int = Function (units :/: Unitless)
  function ./. value = function ./. Float.fromInt value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function units2) (Function units3)

instance Division' (Qty units1) (Function units2) where
  type Qty units1 ./. Function units2 = Function (units1 :/: units2)
  value ./. function = constant value ./. function

instance
  Units.Quotient Unitless units1 units2 =>
  Division Int (Function units1) (Function units2)

instance Division' Int (Function units) where
  type Int ./. Function units = Function (Unitless :/: units)
  value ./. function = Float.fromInt value ./. function

evaluateAt :: Uv.Point -> Function units -> Qty units
evaluateAt uv function =
  case function of
    Function f -> evaluateAtImpl uv f
    Zero -> Qty.zero
    Constant x -> x
    Coerce f -> Units.coerce (evaluateAt uv f)
    Parameter U -> Point2d.xCoordinate uv
    Parameter V -> Point2d.yCoordinate uv
    Negated f -> negate (evaluateAt uv f)
    Sum f1 f2 -> evaluateAt uv f1 + evaluateAt uv f2
    Difference f1 f2 -> evaluateAt uv f1 - evaluateAt uv f2
    Product' f1 f2 -> evaluateAt uv f1 .*. evaluateAt uv f2
    Quotient' f1 f2 -> evaluateAt uv f1 ./. evaluateAt uv f2
    Squared' f -> Qty.squared' (evaluateAt uv f)
    SquareRoot f -> Qty.sqrt (evaluateAt uv f)
    Sin f -> Angle.sin (evaluateAt uv f)
    Cos f -> Angle.cos (evaluateAt uv f)

pointOn :: Function units -> Uv.Point -> Qty units
pointOn function uv = evaluateAt uv function

segmentBounds :: Uv.Bounds -> Function units -> Range units
segmentBounds uv function =
  case function of
    Function f -> segmentBoundsImpl uv f
    Zero -> Range.constant Qty.zero
    Constant x -> Range.constant x
    Coerce f -> Units.coerce (segmentBounds uv f)
    Parameter U -> Bounds2d.xCoordinate uv
    Parameter V -> Bounds2d.yCoordinate uv
    Negated f -> negate (segmentBounds uv f)
    Sum f1 f2 -> segmentBounds uv f1 + segmentBounds uv f2
    Difference f1 f2 -> segmentBounds uv f1 - segmentBounds uv f2
    Product' f1 f2 -> segmentBounds uv f1 .*. segmentBounds uv f2
    Quotient' f1 f2 -> segmentBounds uv f1 ./. segmentBounds uv f2
    Squared' f -> Range.squared' (segmentBounds uv f)
    SquareRoot f -> Range.sqrt (segmentBounds uv f)
    Sin f -> Range.sin (segmentBounds uv f)
    Cos f -> Range.cos (segmentBounds uv f)

boundsOn :: Function units -> Uv.Bounds -> Range units
boundsOn function uvBounds = segmentBounds uvBounds function

derivative :: Parameter -> Function units -> Function units
derivative varyingParameter function =
  case function of
    Function f -> derivativeImpl varyingParameter f
    Zero -> zero
    Constant _ -> zero
    Coerce f -> Units.coerce (derivative varyingParameter f)
    Parameter p -> if p == varyingParameter then constant 1.0 else zero
    Negated f -> negate (derivative varyingParameter f)
    Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
    Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
    Product' f1 f2 -> derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
    Quotient' f1 f2 ->
      (derivative varyingParameter f1 .*. f2 - f1 .*. derivative varyingParameter f2)
        .!/.! squared' f2
    Squared' f -> 2 * f .*. derivative varyingParameter f
    SquareRoot f -> derivative varyingParameter f / (2 * sqrt f)
    Sin f -> cos f * Angle.unitless (derivative varyingParameter f)
    Cos f -> negate (sin f) * Angle.unitless (derivative varyingParameter f)

derivativeIn :: Uv.Direction -> Function units -> Function units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: Function units
zero = Zero

constant :: Qty units -> Function units
constant value = if value == Qty.zero then Zero else Constant value

parameter :: Parameter -> Function Unitless
parameter = Parameter

wrap :: Interface function units => function -> Function units
wrap = Function

squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared function = Units.specialize (squared' function)

squared' :: Function units -> Function (units :*: units)
squared' Zero = Zero
squared' (Constant x) = Constant (x .*. x)
squared' (Negated f) = squared' f
squared' (Cos f) = Units.unspecialize (cosSquared f)
squared' (Sin f) = Units.unspecialize (sinSquared f)
squared' function = Squared' function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2 * f)

sqrt :: Units.Squared units1 units2 => Function units2 -> Function units1
sqrt Zero = Zero
sqrt (Constant x) = Constant (Qty.sqrt x)
sqrt function = SquareRoot function

sin :: Function Radians -> Function Unitless
sin Zero = Zero
sin (Constant x) = constant (Angle.sin x)
sin function = Sin function

cos :: Function Radians -> Function Unitless
cos Zero = Constant 1.0
cos (Constant x) = constant (Angle.cos x)
cos function = Cos function

data CurveOnSurface units where
  CurveOnSurface ::
    Curve2d.Interface curve Uv.Coordinates =>
    curve ->
    Function units ->
    CurveOnSurface units

deriving instance Show (CurveOnSurface units)

instance Curve1d.Interface (CurveOnSurface units) units where
  evaluateAtImpl t (CurveOnSurface uvCurve function) =
    evaluateAt (Curve2d.evaluateAtImpl t uvCurve) function

  segmentBoundsImpl t (CurveOnSurface uvCurve function) =
    segmentBounds (Curve2d.segmentBoundsImpl t uvCurve) function

  derivativeImpl (CurveOnSurface uvCurve function) = do
    let fU = derivative U function
    let fV = derivative V function
    let uvT = Curve2d.derivativeImpl uvCurve
    let uT = VectorCurve2d.xComponent uvT
    let vT = VectorCurve2d.yComponent uvT
    Curve1d.wrap (CurveOnSurface uvCurve fU) * uT + Curve1d.wrap (CurveOnSurface uvCurve fV) * vT

curveOnSurface :: Curve2d Uv.Coordinates -> Function units -> Curve1d units
curveOnSurface uvCurve function = Curve1d.wrap (CurveOnSurface uvCurve function)

isZero :: Tolerance units => Function units -> Bool
isZero function = List.all (~= Qty.zero) (Bounds2d.sample (pointOn function) Uv.domain)

data ZerosError
  = ZeroEverywhere
  | HigherOrderIntersection
  deriving (Eq, Show, Error)

zeros :: Tolerance units => Function units -> Result ZerosError Zeros
zeros Zero = Error ZeroEverywhere
zeros (Constant value) = if value ~= Qty.zero then Error ZeroEverywhere else Ok Zeros.empty
zeros f | isZero f = Error ZeroEverywhere
zeros f = Result.do
  let fu = derivative U f
  let fv = derivative V f
  let fuu = derivative U fu
  let fvv = derivative V fv
  let fuv = derivative V fu
  let derivatives = Derivatives{f, fu, fv, fuu, fvv, fuv}
  let (boundaryEdges, boundaryPoints) = findBoundarySolutions f
  (tangentSolutions, tangentExclusions, saddleRegions) <- findTangentSolutions derivatives boundaryEdges boundaryPoints Uv.domain U [] []
  (crossingSolutions, _) <- findCrossingSolutions derivatives boundaryEdges boundaryPoints Uv.domain U tangentExclusions saddleRegions
  -- TODO report tangent/crossing curves at domain edges when recognized
  -- TODO rename 'solutions' to 'zeros'
  finalizeZeros f (PartialZeros.merge tangentSolutions crossingSolutions)

data Derivatives units = Derivatives
  { f :: Function units
  , fu :: Function units
  , fv :: Function units
  , fuu :: Function units
  , fvv :: Function units
  , fuv :: Function units
  }

findTangentSolutions ::
  Tolerance units =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  Uv.Parameter ->
  List Uv.Bounds ->
  List SaddleRegion ->
  Result ZerosError (PartialZeros, List Uv.Bounds, List SaddleRegion)
findTangentSolutions derivatives boundaryEdges boundaryPoints uvBounds bisectionParameter exclusions saddleRegions = do
  let Derivatives{f, fu, fv} = derivatives
  let fBounds = segmentBounds uvBounds f
  let fuBounds = segmentBounds uvBounds fu
  let fvBounds = segmentBounds uvBounds fv
  if
    -- The function is non-zero for this subdomain, so no solutions
    | not (fBounds ^ Qty.zero) -> Ok (PartialZeros.empty, [], [])
    -- Derivative with respect to U is non-zero for this subdomain, so no tangent solutions
    | not (fuBounds ^ Qty.zero) -> Ok (PartialZeros.empty, [], [])
    -- Derivative with respect to V is non-zero for this subdomain, so no tangent solutions
    | not (fvBounds ^ Qty.zero) -> Ok (PartialZeros.empty, [], [])
    -- We're within an existing exclusion region from a previous solution, so no additional solutions
    | List.any (Bounds2d.contains uvBounds) exclusions -> Ok (PartialZeros.empty, [], [])
    -- We're within an existing saddle region from a previous solution, so no additional solutions
    | List.any (SaddleRegion.exclusion >> Bounds2d.contains uvBounds) saddleRegions -> Ok (PartialZeros.empty, [], [])
    -- Try to find a tangent point (saddle or otherwise)
    | Just result <- tangentPointSolution derivatives boundaryPoints uvBounds exclusions saddleRegions -> result
    -- TODO tangent curve solutions
    | otherwise -> Result.do
        let (bounds1, bounds2) = Uv.bisect bisectionParameter uvBounds
        let nextBisectionParameter = Uv.cycle bisectionParameter
        (solutions1, exclusions1, saddleRegions1) <-
          findTangentSolutions
            derivatives
            boundaryEdges
            boundaryPoints
            bounds1
            nextBisectionParameter
            (List.filter (affects bounds1) exclusions)
            (List.filter (SaddleRegion.exclusion >> affects bounds1) saddleRegions)
        (solutions2, exclusions2, saddleRegions2) <-
          findTangentSolutions
            derivatives
            boundaryEdges
            boundaryPoints
            bounds2
            nextBisectionParameter
            (List.filter (affects bounds2) (exclusions1 + exclusions))
            (List.filter (SaddleRegion.exclusion >> affects bounds2) (saddleRegions1 + saddleRegions))
        Ok
          ( PartialZeros.merge solutions1 solutions2
          , exclusions1 + exclusions2
          , saddleRegions1 + saddleRegions2
          )

findCrossingSolutions ::
  Tolerance units =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  Uv.Parameter ->
  List Uv.Bounds ->
  List SaddleRegion ->
  Result ZerosError (PartialZeros, List Uv.Bounds)
findCrossingSolutions derivatives boundaryEdges boundaryPoints uvBounds bisectionParameter exclusions saddleRegions = do
  let Derivatives{f} = derivatives
  let fBounds = segmentBounds uvBounds f
  if
    -- The function is non-zero for this subdomain, so no solutions
    | not (fBounds ^ Qty.zero) -> Ok (PartialZeros.empty, [])
    -- We're within an existing exclusion region from a previous solution, so no additional solutions
    | List.any (Bounds2d.contains uvBounds) exclusions -> Ok (PartialZeros.empty, [])
    -- We're within an existing saddle region from a previous solution, so no additional solutions
    | List.any (SaddleRegion.exclusion >> Bounds2d.contains uvBounds) saddleRegions -> Ok (PartialZeros.empty, [])
    -- Try to find a general crossing curve solution and report it if it exists
    | Just solution <- generalSolution derivatives uvBounds exclusions -> Ok solution
    -- Try to find a horizontal crossing curve solution and report it if it exists
    | Just solution <- horizontalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions -> Ok solution
    -- Try to find a vertical crossing curve solution and report it if it exists
    | Just solution <- verticalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions -> Ok solution
    -- Check if we've found a point where all derivatives are zero,
    -- indicating that there's a higher-order solution that we can't solve for
    | fBounds ~= Qty.zero && allDerivativesZero uvBounds derivatives -> Error HigherOrderIntersection
    -- If we haven't been able to identify a specific form of solution within this subdomain,
    -- then we need to recurse into subdomains
    | otherwise -> Result.do
        let (bounds1, bounds2) = Uv.bisect bisectionParameter uvBounds
        let nextBisectionParameter = Uv.cycle bisectionParameter
        (solutions1, exclusions1) <-
          findCrossingSolutions
            derivatives
            boundaryEdges
            boundaryPoints
            bounds1
            nextBisectionParameter
            (List.filter (affects bounds1) exclusions)
            (List.filter (SaddleRegion.exclusion >> affects bounds1) saddleRegions)
        (solutions2, exclusions2) <-
          findCrossingSolutions
            derivatives
            boundaryEdges
            boundaryPoints
            bounds2
            nextBisectionParameter
            (List.filter (affects bounds2) (exclusions1 + exclusions))
            (List.filter (SaddleRegion.exclusion >> affects bounds2) saddleRegions)
        Ok
          ( PartialZeros.merge solutions1 solutions2
          , exclusions1 + exclusions2
          )

data BoundaryEdges = BoundaryEdges
  { leftEdgeIsSolution :: Bool
  , rightEdgeIsSolution :: Bool
  , bottomEdgeIsSolution :: Bool
  , topEdgeIsSolution :: Bool
  }
  deriving (Show)

data BoundaryPoint = BoundaryPoint
  { point :: Uv.Point
  , edgeSign :: Sign
  , rootOrder :: Int
  , rootSign :: Sign
  }
  deriving (Show)

boundaryEdge :: Uv.Point -> Uv.Direction -> Curve2d Uv.Coordinates
boundaryEdge startPoint direction =
  Line2d.from startPoint (startPoint + Direction2d.vector direction)

leftEdge :: Curve2d Uv.Coordinates
leftEdge = boundaryEdge (Point2d.xy 0.0 0.0) Direction2d.y

rightEdge :: Curve2d Uv.Coordinates
rightEdge = boundaryEdge (Point2d.xy 1.0 0.0) Direction2d.y

bottomEdge :: Curve2d Uv.Coordinates
bottomEdge = boundaryEdge (Point2d.xy 0.0 0.0) Direction2d.x

topEdge :: Curve2d Uv.Coordinates
topEdge = boundaryEdge (Point2d.xy 0.0 1.0) Direction2d.x

findBoundarySolutions :: Tolerance units => Function units -> (BoundaryEdges, List BoundaryPoint)
findBoundarySolutions f = do
  let (leftEdgeIsSolution, leftPoints) = edgeSolutions f leftEdge Negative
  let (rightEdgeIsSolution, rightPoints) = edgeSolutions f rightEdge Positive
  let (bottomEdgeIsSolution, bottomPoints) = edgeSolutions f bottomEdge Negative
  let (topEdgeIsSolution, topPoints) = edgeSolutions f topEdge Positive
  let boundaryEdges =
        BoundaryEdges
          { leftEdgeIsSolution
          , rightEdgeIsSolution
          , bottomEdgeIsSolution
          , topEdgeIsSolution
          }
  let boundaryPoints = List.concat [leftPoints, rightPoints, bottomPoints, topPoints]
  (boundaryEdges, boundaryPoints)

edgeSolutions :: Tolerance units => Function units -> Curve2d Uv.Coordinates -> Sign -> (Bool, List BoundaryPoint)
edgeSolutions f edgeCurve edgeSign =
  case Curve1d.zeros (Curve1d.wrap (CurveOnSurface edgeCurve f)) of
    -- TODO classify edge curve as crossing or tangent:
    --   - Find zeros of partial derivative of f perpendicular to curve
    --   - If zero everywhere, then tangent curve
    --   - If no roots, then crossing curve
    --   - If there *are* roots, then those are tangent/saddle points of some sort
    Curve1d.ZeroEverywhere -> (True, [])
    Curve1d.Zeros roots -> do
      let toBoundaryPoint root =
            BoundaryPoint
              { point = Curve2d.pointOn edgeCurve (Curve1d.Root.value root)
              , edgeSign
              , rootOrder = Curve1d.Root.order root
              , rootSign = Curve1d.Root.sign root
              }
      (False, List.map toBoundaryPoint roots)

affects :: Uv.Bounds -> Uv.Bounds -> Bool
affects bounds exclusion = overlaps exclusion (expandBounds bounds)

overlaps :: Uv.Bounds -> Uv.Bounds -> Bool
overlaps bounds1 bounds2 = Bounds2d.overlap bounds1 bounds2 > Qty.zero

expandRangeBy :: Float -> Range Unitless -> Range Unitless
expandRangeBy factor (Range low high) = do
  let halfWidth = factor * (high - low)
  let expandedLow = Float.max (low - halfWidth) 0.0
  let expandedHigh = Float.min (high + halfWidth) 1.0
  Range.from expandedLow expandedHigh

expandBoundsBy :: Float -> Uv.Bounds -> Uv.Bounds
expandBoundsBy factor uvBounds = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  Bounds2d.xy (expandRangeBy factor uRange) (expandRangeBy factor vRange)

expandRange :: Range Unitless -> Range Unitless
expandRange = expandRangeBy 0.5

expandBounds :: Uv.Bounds -> Uv.Bounds
expandBounds = expandBoundsBy 0.5

generalSolution ::
  Tolerance units =>
  Derivatives units ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (PartialZeros, List Uv.Bounds)
generalSolution derivatives uvBounds exclusions = do
  let Derivatives{f, fu, fv} = derivatives
  let fuBounds = segmentBounds uvBounds fu
  let fvBounds = segmentBounds uvBounds fv
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  if
    | List.any (overlaps uvBounds) exclusions -> Nothing
    | resolved fuBounds && resolved fvBounds -> do
        let signAt u v = Qty.sign (evaluateAt (Point2d.xy u v) f)
        let solution =
              case (signAt minU minV, signAt maxU minV, signAt minU maxV, signAt maxU maxV) of
                (Positive, Positive, Positive, Positive) -> PartialZeros.empty
                (Negative, Negative, Negative, Negative) -> PartialZeros.empty
                (Positive, Positive, Negative, Negative) -> rightwardsSolution f fu fv uvBounds
                (Negative, Negative, Positive, Positive) -> leftwardsSolution f fu fv uvBounds
                (Positive, Negative, Positive, Negative) -> downwardsSolution f fu fv uvBounds
                (Negative, Positive, Negative, Positive) -> upwardsSolution f fu fv uvBounds
                -- One positive corner
                (Positive, Negative, Negative, Negative) -> do
                  -- Bottom left positive
                  let startV = solveVertically f minU maxV minV
                  let endU = solveHorizontally f maxU minU minV
                  let startBoundary = Boundary.left uvBounds
                  let endBoundary = Boundary.bottom uvBounds
                  if endU - minU >= startV - minV
                    then crossingSolution (endU == minU) startBoundary endBoundary (horizontalCurve f fu fv minU endU startV minV)
                    else crossingSolution (startV == minV) startBoundary endBoundary (verticalCurve f fu fv endU minU startV minV)
                (Negative, Positive, Negative, Negative) -> do
                  -- Bottom right positive
                  let startU = solveHorizontally f minU maxU minV
                  let endV = solveVertically f maxU maxV minV
                  let startBoundary = Boundary.bottom uvBounds
                  let endBoundary = Boundary.right uvBounds
                  if maxU - startU >= endV - minV
                    then crossingSolution (startU == maxU) startBoundary endBoundary (horizontalCurve f fu fv startU maxU endV minV)
                    else crossingSolution (endV == minV) startBoundary endBoundary (verticalCurve f fu fv startU maxU minV endV)
                (Negative, Negative, Positive, Negative) -> do
                  -- Top left positive
                  let startU = solveHorizontally f maxU minU maxV
                  let endV = solveVertically f minU minV maxV
                  let startBoundary = Boundary.top uvBounds
                  let endBoundary = Boundary.left uvBounds
                  if startU - minU >= maxV - endV
                    then crossingSolution (startU == minU) startBoundary endBoundary (horizontalCurve f fu fv startU minU endV maxV)
                    else crossingSolution (endV == maxV) startBoundary endBoundary (verticalCurve f fu fv startU minU maxV endV)
                (Negative, Negative, Negative, Positive) -> do
                  -- Top right positive
                  let startV = solveVertically f maxU minV maxV
                  let endU = solveHorizontally f minU maxU maxV
                  let startBoundary = Boundary.right uvBounds
                  let endBoundary = Boundary.top uvBounds
                  if maxU - endU >= maxV - startV
                    then crossingSolution (endU == maxU) startBoundary endBoundary (horizontalCurve f fu fv maxU endU startV maxV)
                    else crossingSolution (startV == maxV) startBoundary endBoundary (verticalCurve f fu fv endU maxU startV maxV)
                -- One negative corner
                (Negative, Positive, Positive, Positive) -> do
                  -- Bottom left negative
                  let endV = solveVertically f minU minV maxV
                  let startU = solveHorizontally f minU maxU minV
                  let startBoundary = Boundary.bottom uvBounds
                  let endBoundary = Boundary.left uvBounds
                  if startU - minU >= endV - minV
                    then crossingSolution (startU == minU) startBoundary endBoundary (horizontalCurve f fu fv startU minU minV endV)
                    else crossingSolution (endV == minV) startBoundary endBoundary (verticalCurve f fu fv minU startU minV endV)
                (Positive, Negative, Positive, Positive) -> do
                  -- Bottom right negative
                  let startV = solveVertically f maxU minV maxV
                  let endU = solveHorizontally f maxU minU minV
                  let startBoundary = Boundary.right uvBounds
                  let endBoundary = Boundary.bottom uvBounds
                  if maxU - endU >= startV - minV
                    then crossingSolution (endU == maxU) startBoundary endBoundary (horizontalCurve f fu fv maxU endU minV startV)
                    else crossingSolution (startV == minV) startBoundary endBoundary (verticalCurve f fu fv maxU endU startV minV)
                (Positive, Positive, Negative, Positive) -> do
                  -- Top left negative
                  let startV = solveVertically f minU maxV minV
                  let endU = solveHorizontally f minU maxU maxV
                  let startBoundary = Boundary.left uvBounds
                  let endBoundary = Boundary.top uvBounds
                  if endU - minU >= maxV - startV
                    then crossingSolution (endU == minU) startBoundary endBoundary (horizontalCurve f fu fv minU endU maxV startV)
                    else crossingSolution (startV == maxV) startBoundary endBoundary (verticalCurve f fu fv minU endU startV maxV)
                (Positive, Positive, Positive, Negative) -> do
                  -- Top right negative
                  let startU = solveHorizontally f maxU minU maxV
                  let endV = solveVertically f maxU maxV minV
                  let startBoundary = Boundary.top uvBounds
                  let endBoundary = Boundary.right uvBounds
                  if maxU - startU >= maxV - endV
                    then crossingSolution (startU == maxU) startBoundary endBoundary (horizontalCurve f fu fv startU maxU maxV endV)
                    else crossingSolution (endV == maxV) startBoundary endBoundary (verticalCurve f fu fv maxU startU maxV endV)
                -- Shouldn't happen
                (Negative, Positive, Positive, Negative) -> internalError "Inconsistent derivatives"
                (Positive, Negative, Negative, Positive) -> internalError "Inconsistent derivatives"
        Just (solution, [uvBounds])
    | otherwise -> Nothing

resolved :: Range units -> Bool
resolved range = Qty.abs (Range.resolution range) >= 0.5

isStrictlyInside :: Uv.Bounds -> BoundaryPoint -> Bool
isStrictlyInside bounds (BoundaryPoint{point}) = Bounds2d.inclusion point bounds > 0.0

horizontalSolution ::
  Tolerance units =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (PartialZeros, List Uv.Bounds)
horizontalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  let Derivatives{f, fu, fv} = derivatives
  let BoundaryEdges{bottomEdgeIsSolution, topEdgeIsSolution} = boundaryEdges
  let expandedVRange = expandRange vRange
  let Range vBottom vTop = expandedVRange
  let expandedBounds = Bounds2d.xy uRange expandedVRange
  let fvResolution = Range.resolution (segmentBounds expandedBounds fv)
  let sliceSign v uSubRange = rangeSign (boundsOn f (Bounds2d.xy uSubRange (Range.constant v)))
  let bottomSign = Range.resolve (sliceSign vBottom) uRange
  let topSign = Range.resolve (sliceSign vTop) uRange
  let trimmedBounds =
        case List.filter (isStrictlyInside expandedBounds) boundaryPoints of
          [] -> Just expandedBounds
          List.One (BoundaryPoint{point, edgeSign, rootOrder, rootSign}) -> do
            let u0 = Point2d.xCoordinate point
            case (rootOrder, edgeSign, rootSign * Qty.sign fvResolution) of
              (Int.Even, Negative, Negative) -> Just (Bounds2d.xy (Range.from u0 maxU) (Range.from minV vTop))
              (Int.Even, Negative, Positive) -> Just (Bounds2d.xy (Range.from minU u0) (Range.from minV vTop))
              (Int.Even, Positive, Negative) -> Just (Bounds2d.xy (Range.from minU u0) (Range.from vBottom maxV))
              (Int.Even, Positive, Positive) -> Just (Bounds2d.xy (Range.from u0 maxU) (Range.from vBottom maxV))
              (Int.Odd, Negative, Negative) -> Just (Bounds2d.xy (Range.from minU maxU) (Range.from minV vTop))
              (Int.Odd, Negative, Positive) -> Nothing
              (Int.Odd, Positive, Negative) -> Nothing
              (Int.Odd, Positive, Positive) -> Just (Bounds2d.xy (Range.from minU maxU) (Range.from vBottom maxV))
          List.TwoOrMore -> Nothing
  if
    | List.any (overlaps expandedBounds) exclusions -> Nothing
    | Qty.abs fvResolution >= 0.5
    , (bottomEdgeIsSolution && minV == 0.0) || (topEdgeIsSolution && maxV == 1.0) ->
        Just (PartialZeros.empty, [])
    | fvResolution >= 0.5
    , bottomSign == Resolved Negative
    , topSign == Resolved Positive
    , Just solutionBounds <- trimmedBounds ->
        Just (leftwardsSolution f fu fv solutionBounds, [expandedBounds])
    | fvResolution <= -0.5
    , bottomSign == Resolved Positive
    , topSign == Resolved Negative
    , Just solutionBounds <- trimmedBounds ->
        Just (rightwardsSolution f fu fv solutionBounds, [expandedBounds])
    | otherwise -> Nothing

verticalSolution ::
  Tolerance units =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (PartialZeros, List Uv.Bounds)
verticalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  let Derivatives{f, fu, fv} = derivatives
  let BoundaryEdges{leftEdgeIsSolution, rightEdgeIsSolution} = boundaryEdges
  let expandedURange = expandRange uRange
  let Range uLeft uRight = expandedURange
  let expandedBounds = Bounds2d.xy expandedURange vRange
  let fuResolution = Range.resolution (segmentBounds expandedBounds fu)
  let sliceSign u vSubRange = rangeSign (boundsOn f (Bounds2d.xy (Range.constant u) vSubRange))
  let leftSign = Range.resolve (sliceSign uLeft) vRange
  let rightSign = Range.resolve (sliceSign uRight) vRange
  let trimmedBounds =
        case List.filter (isStrictlyInside expandedBounds) boundaryPoints of
          [] -> Just expandedBounds
          List.One (BoundaryPoint{point, edgeSign, rootOrder, rootSign}) -> do
            let v0 = Point2d.yCoordinate point
            case (rootOrder, edgeSign, rootSign * Qty.sign fuResolution) of
              (Int.Even, Negative, Negative) -> Just (Bounds2d.xy (Range.from minU uRight) (Range.from v0 maxV))
              (Int.Even, Negative, Positive) -> Just (Bounds2d.xy (Range.from minU uRight) (Range.from minV v0))
              (Int.Even, Positive, Negative) -> Just (Bounds2d.xy (Range.from uLeft maxU) (Range.from minV v0))
              (Int.Even, Positive, Positive) -> Just (Bounds2d.xy (Range.from uLeft maxU) (Range.from v0 maxV))
              (Int.Odd, Negative, Negative) -> Just (Bounds2d.xy (Range.from minU uRight) (Range.from minV maxV))
              (Int.Odd, Negative, Positive) -> Nothing
              (Int.Odd, Positive, Negative) -> Nothing
              (Int.Odd, Positive, Positive) -> Just (Bounds2d.xy (Range.from uLeft maxU) (Range.from minV maxV))
          List.TwoOrMore -> Nothing
  if
    | List.any (overlaps expandedBounds) exclusions -> Nothing
    | Qty.abs fuResolution >= 0.5
    , (leftEdgeIsSolution && minU == 0.0) || (rightEdgeIsSolution && maxU == 1.0) ->
        Just (PartialZeros.empty, [])
    | fuResolution >= 0.5
    , leftSign == Resolved Negative
    , rightSign == Resolved Positive
    , Just solutionBounds <- trimmedBounds ->
        Just (upwardsSolution f fu fv solutionBounds, [expandedBounds])
    | fuResolution <= -0.5
    , leftSign == Resolved Positive
    , rightSign == Resolved Negative
    , Just solutionBounds <- trimmedBounds ->
        Just (downwardsSolution f fu fv solutionBounds, [expandedBounds])
    | otherwise -> Nothing

rightwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  PartialZeros
rightwardsSolution f fu fv uvBounds = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  crossingSolution (minU == maxU) (Boundary.Left minU vRange) (Boundary.Right maxU vRange) $
    horizontalCurve f fu fv minU maxU maxV minV

leftwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  PartialZeros
leftwardsSolution f fu fv uvBounds = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  crossingSolution (minU == maxU) (Boundary.Right maxU vRange) (Boundary.Left minU vRange) $
    horizontalCurve f fu fv maxU minU minV maxV

upwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  PartialZeros
upwardsSolution f fu fv uvBounds = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  crossingSolution (minV == maxV) (Boundary.Bottom uRange minV) (Boundary.Top uRange maxV) $
    verticalCurve f fu fv minU maxU minV maxV

downwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  PartialZeros
downwardsSolution f fu fv uvBounds = do
  let (uRange, vRange) = Bounds2d.coordinates uvBounds
  let (minU, maxU) = Range.endpoints uRange
  let (minV, maxV) = Range.endpoints vRange
  crossingSolution (minV == maxV) (Boundary.Top uRange maxV) (Boundary.Bottom uRange minV) $
    verticalCurve f fu fv maxU minU maxV minV

crossingSolution ::
  Bool ->
  Boundary ->
  Boundary ->
  Curve2d Uv.Coordinates ->
  PartialZeros
crossingSolution isDegenerate start end curve
  | isDegenerate = PartialZeros.degenerateCrossingCurve start end
  | otherwise = PartialZeros.crossingCurve start end curve

horizontalCurve ::
  Function units ->
  Function units ->
  Function units ->
  Float ->
  Float ->
  Float ->
  Float ->
  Curve2d Uv.Coordinates
horizontalCurve f fu fv uStart uEnd vLow vHigh =
  Curve2d.wrap (HorizontalCurve{f, dvdu = -fu / fv, uStart, uEnd, vLow, vHigh})

verticalCurve ::
  Function units ->
  Function units ->
  Function units ->
  Float ->
  Float ->
  Float ->
  Float ->
  Curve2d Uv.Coordinates
verticalCurve f fu fv uLow uHigh vStart vEnd =
  Curve2d.wrap (VerticalCurve{f, dudv = -fv / fu, uLow, uHigh, vStart, vEnd})

hasZero :: Uv.Bounds -> Function units -> Bool
hasZero uvBounds function = Range.includes Qty.zero (segmentBounds uvBounds function)

tangentPointSolution ::
  Tolerance units =>
  Derivatives units ->
  List BoundaryPoint ->
  Uv.Bounds ->
  List Uv.Bounds ->
  List SaddleRegion ->
  Maybe (Result ZerosError (PartialZeros, List Uv.Bounds, List SaddleRegion))
tangentPointSolution derivatives boundaryPoints uvBounds exclusions saddleRegions = do
  let Derivatives{f, fu, fv, fuu, fvv, fuv} = derivatives
  let expandedBounds = expandBounds uvBounds
  let searchBounds = expandBoundsBy 0.05 uvBounds
  let fuuBounds = segmentBounds expandedBounds fuu
  let fvvBounds = segmentBounds expandedBounds fvv
  let fuvBounds = segmentBounds expandedBounds fuv
  let determinantResolution = Range.resolution (fuuBounds .*. fvvBounds - fuvBounds .*. fuvBounds)
  let isIncludedTangentPoint (BoundaryPoint{point}) =
        Bounds2d.includes point searchBounds
          && evaluateAt point fu ~= Qty.zero
          && evaluateAt point fv ~= Qty.zero
  let includedTangentBoundaryPoint = List.find isIncludedTangentPoint boundaryPoints
  let isSolution uv = hasZero uv fu && hasZero uv fv
  if
    | List.any (overlaps expandedBounds) exclusions -> Nothing
    | List.any (SaddleRegion.exclusion >> overlaps expandedBounds) saddleRegions -> Nothing
    -- If second derivatives determinant is not definitely non-zero, then abort
    | Qty.abs determinantResolution < 0.5 -> Nothing
    -- Otherwise, we know there can be only one tangent point
    | Just (BoundaryPoint{point}) <- includedTangentBoundaryPoint ->
        -- The tangent boundary point included in the expanded bounds
        -- must be the tangent point, so we shouldn't report it again as a solution
        -- but we _can_ now report an exclusion region or saddle point
        case Qty.sign determinantResolution of
          -- Positive determinant: non-saddle tangent point,
          -- so report an exclusion region around it
          Positive -> Just (Ok (PartialZeros.empty, [expandedBounds], []))
          -- Negative determinant: saddle tangent point,
          -- so report the existing boundary tangent point as
          -- an saddle point with the current expanded bounds
          -- as its associated region
          Negative -> Just (Ok (saddlePointSolution derivatives point expandedBounds))
    | Just point <- Bounds2d.find isSolution searchBounds
    , evaluateAt point f ~= Qty.zero ->
        -- We've found a tangent point! Now to check if it's a saddle point
        case Qty.sign determinantResolution of
          Positive -> do
            -- Non-saddle tangent point
            -- Note that fuu and fvv must be either both positive or both negative
            -- to reach this code path, so we can take the sign of either one
            -- to determine the sign of the tangent point
            let sign = Qty.sign (Range.minValue fuuBounds)
            Just (Ok (PartialZeros.tangentPoint point sign, [expandedBounds], []))
          Negative -> do
            -- Saddle point
            Just (Ok (saddlePointSolution derivatives point expandedBounds))
    | otherwise -> Nothing

saddlePointSolution :: Derivatives units -> Uv.Point -> Uv.Bounds -> (PartialZeros, List Uv.Bounds, List SaddleRegion)
saddlePointSolution derivatives point expandedBounds = do
  let saddleRegion = saddlePointRegion derivatives point expandedBounds
  (PartialZeros.saddleRegion saddleRegion, [], [saddleRegion])

maxRadiusForComponent :: Float -> Float -> Range Unitless -> Float
maxRadiusForComponent component origin (Range low high)
  | component < 0.0 = if low == 0.0 then Float.infinity else (low - origin) / component
  | component > 0.0 = if high == 1.0 then Float.infinity else (high - origin) / component
  | otherwise = Float.infinity

saddlePointRegion :: Derivatives units -> Uv.Point -> Uv.Bounds -> SaddleRegion
saddlePointRegion derivatives point expandedBounds = do
  let Derivatives{f, fuu, fuv, fvv} = derivatives
  let fuuValue = evaluateAt point fuu
  let fuvValue = evaluateAt point fuv
  let fvvValue = evaluateAt point fvv
  let (u0, v0) = Point2d.coordinates point
  let (uRange, vRange) = Bounds2d.coordinates expandedBounds
  let sqrtD = Qty.sqrt' (fuvValue .*. fuvValue - fuuValue .*. fvvValue)
  let (d1, d2) =
        if Qty.abs fuuValue >= Qty.abs fvvValue
          then
            ( Vector2d.normalize (Vector2d (-fuvValue + sqrtD) fuuValue)
            , Vector2d.normalize (Vector2d (-fuvValue - sqrtD) fuuValue)
            )
          else
            ( Vector2d.normalize (Vector2d fvvValue (-fuvValue + sqrtD))
            , Vector2d.normalize (Vector2d fvvValue (-fuvValue - sqrtD))
            )
  let xDirection = Direction2d.unsafe (Vector2d.normalize (d1 + d2))
  let xAxis = Axis2d.through point xDirection
  let frame = Frame2d.fromXAxis xAxis
  let maxRadiusForVector (Vector2d u v) =
        Float.min
          (maxRadiusForComponent u u0 uRange)
          (maxRadiusForComponent v v0 vRange)
  let radius = Float.min (maxRadiusForVector d1) (maxRadiusForVector d2)
  let Vector2d u1 v1 = Vector2d.relativeTo frame d1
  let halfWidth = Float.abs (radius * u1)
  let halfHeight = Float.abs (radius * v1)
  let f_xy = reparameterize frame f
  let fx = derivative U f_xy
  let fy = derivative V f_xy
  let fxx = derivative U fx
  let fxy = derivative V fx
  let fyy = derivative V fy
  let fxxx = derivative U fxx
  let fxxy = derivative V fxx
  let fxyy = derivative V fxy
  let fyyy = derivative V fyy
  let fxxValue = evaluateAt Point2d.origin fxx
  let fyyValue = evaluateAt Point2d.origin fyy
  let fxxxValue = evaluateAt Point2d.origin fxxx
  let fxxyValue = evaluateAt Point2d.origin fxxy
  let fxyyValue = evaluateAt Point2d.origin fxyy
  let fyyyValue = evaluateAt Point2d.origin fyyy
  let positiveA = Qty.sqrt' (-fxxValue .*. fyyValue) / Qty.abs fyyValue
  let negativeA = -positiveA
  let b a = -(fyyyValue * a ** 3 + 3 * fxyyValue * a ** 2 + 3 * fxxyValue * a + fxxxValue) / (3 * a * fyyValue)
  let positiveSolution = SaddleRegion.Solution{dydx = positiveA, d2ydx2 = b positiveA}
  let negativeSolution = SaddleRegion.Solution{dydx = negativeA, d2ydx2 = b negativeA}
  SaddleRegion
    { frame
    , halfWidth
    , halfHeight
    , positiveSolution
    , negativeSolution
    , exclusion = expandedBounds
    }

reparameterize ::
  Frame2d Uv.Coordinates (Defines Uv.Space) ->
  Function units ->
  Function units
reparameterize localFrame function = Function (Reparameterized localFrame function)

data Reparameterized units where
  Reparameterized ::
    Frame2d Uv.Coordinates (Defines Uv.Space) ->
    Function units ->
    Reparameterized units

deriving instance Show (Reparameterized units)

instance Interface (Reparameterized units) units where
  evaluateAtImpl uvPoint (Reparameterized frame function) =
    evaluateAt (Point2d.placeIn frame uvPoint) function

  segmentBoundsImpl uvBounds (Reparameterized frame function) =
    segmentBounds (Bounds2d.placeIn frame uvBounds) function

  derivativeImpl U (Reparameterized frame function) =
    reparameterize frame (derivativeIn (Frame2d.xDirection frame) function)
  derivativeImpl V (Reparameterized frame function) =
    reparameterize frame (derivativeIn (Frame2d.yDirection frame) function)

-- isTangentCurveByU
--   | segmentBounds vBottomSlice fv ^ Qty.zero = False
--   | segmentBounds vTopSlice fv ^ Qty.zero = False
--   | Qty.abs (Range.resolution (segmentBounds expandedVBounds fvv)) < 0.5 = False
--   | otherwise =
--       let isTangentIntersectionPoint uValue =
--             let fvvalue vValue = evaluateAt (Point2d uValue vValue) fv
--              in case Range.solve fvvalue expandedVRange of
--                   Nothing -> False
--                   Just vValue ->
--                     let uvValue = Point2d uValue vValue
--                      in evaluateAt uvValue f ~= Qty.zero && evaluateAt uvValue fu ~= Qty.zero
--        in List.all isTangentIntersectionPoint (Range.samples uRange)

-- isTangentCurveByV
--   | segmentBounds uLeftSlice fu ^ Qty.zero = False
--   | segmentBounds uRightSlice fu ^ Qty.zero = False
--   | Qty.abs (Range.resolution (segmentBounds expandedUBounds fuu)) < 0.5 = False
--   | otherwise =
--       let isTangentIntersectionPoint vValue =
--             let fuValue uValue = evaluateAt (Point2d uValue vValue) fu
--              in case Range.solve fuValue expandedURange of
--                   Nothing -> False
--                   Just uValue ->
--                     let uvValue = Point2d uValue vValue
--                      in evaluateAt uvValue f ~= Qty.zero && evaluateAt uvValue fv ~= Qty.zero
--        in List.all isTangentIntersectionPoint (Range.samples vRange)

allDerivativesZero ::
  Tolerance units =>
  Uv.Bounds ->
  Derivatives units ->
  Bool
allDerivativesZero uvBounds (Derivatives{fu, fv, fuu, fvv, fuv}) =
  segmentBounds uvBounds fu ~= Qty.zero
    && segmentBounds uvBounds fv ~= Qty.zero
    && segmentBounds uvBounds fuu ~= Qty.zero
    && segmentBounds uvBounds fvv ~= Qty.zero
    && segmentBounds uvBounds fuv ~= Qty.zero

-- TODO: have the tolerance here be much larger
-- (based on the derivative resolution)
-- to avoid expensive bisection near zeros
rangeSign :: Tolerance units => Range units -> Fuzzy Sign
rangeSign range
  | Range.minValue range > ?tolerance = Resolved Positive
  | Range.maxValue range < negate ?tolerance = Resolved Negative
  | otherwise = Unresolved

data HorizontalCurve units = HorizontalCurve
  { f :: Function units
  , dvdu :: Function Unitless
  , uStart :: Float
  , uEnd :: Float
  , vLow :: Float
  , vHigh :: Float
  }
  deriving (Show)

instance Curve2d.Interface (HorizontalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t (HorizontalCurve{f, uStart, uEnd, vLow, vHigh}) = do
    let u = Float.interpolateFrom uStart uEnd t
    let v = solveVertically f u vLow vHigh
    Point2d.xy u v

  segmentBoundsImpl (Range t1 t2) (HorizontalCurve{f, dvdu, uStart, uEnd, vLow, vHigh}) = do
    let u1 = Float.interpolateFrom uStart uEnd t1
    let u2 = Float.interpolateFrom uStart uEnd t2
    let v1 = solveVertically f u1 vLow vHigh
    let v2 = solveVertically f u2 vLow vHigh
    let slopeBounds = segmentBounds (Bounds2d.xy (Range.from u1 u2) (Range.from vLow vHigh)) dvdu
    let vRange = parallelogramBounds u1 u2 v1 v2 slopeBounds
    Bounds2d.xy (Range.from u1 u2) vRange

  derivativeImpl crossingCurve@(HorizontalCurve{dvdu, uStart, uEnd}) = do
    let deltaU = uEnd - uStart
    let dudt = Curve1d.constant deltaU
    let dvdt = deltaU * Curve1d.wrap (CurveOnSurface crossingCurve dvdu)
    VectorCurve2d.xy dudt dvdt

  reverseImpl (HorizontalCurve{f, dvdu, uStart, uEnd, vLow, vHigh}) =
    HorizontalCurve{f, dvdu, uStart = uEnd, uEnd = uStart, vLow, vHigh}

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl Range.unit crossingCurve

  transformByImpl transform crossingCurve =
    Curve2d.wrap (Curve2d.TransformBy transform crossingCurve)

data VerticalCurve units = VerticalCurve
  { f :: Function units
  , dudv :: Function Unitless
  , uLow :: Float
  , uHigh :: Float
  , vStart :: Float
  , vEnd :: Float
  }
  deriving (Show)

instance Curve2d.Interface (VerticalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t (VerticalCurve{f, uLow, uHigh, vStart, vEnd}) = do
    let v = Float.interpolateFrom vStart vEnd t
    let u = solveHorizontally f uLow uHigh v
    Point2d.xy u v

  segmentBoundsImpl (Range t1 t2) (VerticalCurve{f, dudv, uLow, uHigh, vStart, vEnd}) = do
    let v1 = Float.interpolateFrom vStart vEnd t1
    let v2 = Float.interpolateFrom vStart vEnd t2
    let u1 = solveHorizontally f uLow uHigh v1
    let u2 = solveHorizontally f uLow uHigh v2
    let slopeBounds = segmentBounds (Bounds2d.xy (Range.from uLow uHigh) (Range.from v1 v2)) dudv
    let uRange = parallelogramBounds v1 v2 u1 u2 slopeBounds
    Bounds2d.xy uRange (Range.from v1 v2)

  derivativeImpl crossingCurve@(VerticalCurve{dudv, vStart, vEnd}) = do
    let deltaV = vEnd - vStart
    let dvdt = Curve1d.constant deltaV
    let dudt = deltaV * Curve1d.wrap (CurveOnSurface crossingCurve dudv)
    VectorCurve2d.xy dudt dvdt

  reverseImpl (VerticalCurve{f, dudv, uLow, uHigh, vStart, vEnd}) =
    VerticalCurve f dudv uLow uHigh vEnd vStart

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl Range.unit crossingCurve

  transformByImpl transform crossingCurve =
    Curve2d.wrap (Curve2d.TransformBy transform crossingCurve)

solveVertically :: Function units -> Float -> Float -> Float -> Float
solveVertically f u v1 v2 = do
  let valueAt v = evaluateAt (Point2d.xy u v) f
  let bisect vLow vHigh = do
        let vMid = Qty.midpoint vLow vHigh
        let fMid = valueAt vMid
        if
          | vMid == vLow || vMid == vHigh -> vMid
          | fMid < Qty.zero -> bisect vMid vHigh
          | fMid > Qty.zero -> bisect vLow vMid
          | otherwise -> vMid
  if
    | valueAt v1 >= Qty.zero -> v1
    | valueAt v2 <= Qty.zero -> v2
    | otherwise -> bisect v1 v2

solveHorizontally :: Function units -> Float -> Float -> Float -> Float
solveHorizontally f u1 u2 v = do
  let valueAt u = evaluateAt (Point2d.xy u v) f
  let bisect uLow uHigh = do
        let uMid = Qty.midpoint uLow uHigh
        let fMid = valueAt uMid
        if
          | uMid == uLow || uMid == uHigh -> uMid
          | fMid < Qty.zero -> bisect uMid uHigh
          | fMid > Qty.zero -> bisect uLow uMid
          | otherwise -> uMid
  if
    | valueAt u1 >= Qty.zero -> u1
    | valueAt u2 <= Qty.zero -> u2
    | otherwise -> bisect u1 u2

parallelogramBounds ::
  Float ->
  Float ->
  Float ->
  Float ->
  Range Unitless ->
  Range Unitless
parallelogramBounds x1 x2 y1 y2 (Range minSlope maxSlope) = do
  let deltaX = x2 - x1
  let deltaY = y2 - y1
  let deltaXLow = (maxSlope * deltaX - deltaY) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
  let deltaXHigh = (deltaY - minSlope * deltaX) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
  let yLow = y1 + minSlope * deltaXLow
  let yHigh = y1 + maxSlope * deltaXHigh
  Range.hull4 y1 y2 yLow yHigh

finalizeZeros :: Function units -> PartialZeros -> Result ZerosError Zeros
finalizeZeros f partialZeros = Result.do
  let PartialZeros
        { crossingCurves
        , crossingLoops
        , tangentCurves
        , tangentLoops
        , tangentPoints
        , saddleRegions
        } = partialZeros
  let finalizedCrossingCurveResults =
        Maybe.collect (finalizeCrossingCurve f saddleRegions) crossingCurves
  finalizedCrossingCurves <- Result.combine finalizedCrossingCurveResults
  -- Check that there were no degenerate curve segments left
  Debug.assert (List.length finalizedCrossingCurves == List.length crossingCurves)
  Ok
    Zeros
      { crossingCurves = finalizedCrossingCurves
      , crossingLoops
      , tangentCurves = Maybe.collect finalizeTangentCurve tangentCurves
      , tangentLoops = List.map finalizeTangentLoop tangentLoops
      , tangentPoints = List.map finalizeTangentPoint tangentPoints
      , saddlePoints = List.map SaddleRegion.point saddleRegions
      , -- TODO report crossing point solutions at domain corners
        crossingPoints = []
      }

finalizeCrossingCurve ::
  Function units ->
  List SaddleRegion ->
  PartialZeros.CrossingCurve ->
  Maybe (Result ZerosError (NonEmpty (Curve2d Uv.Coordinates)))
finalizeCrossingCurve _ _ (PartialZeros.DegenerateCrossingCurve{}) = Nothing
finalizeCrossingCurve f saddleRegions (PartialZeros.CrossingCurve{segments}) = do
  let firstCurve = NonEmpty.first segments
  let lastCurve = NonEmpty.last segments
  let startPoint = Curve2d.startPoint firstCurve
  let endPoint = Curve2d.endPoint lastCurve
  let regionContaining point = List.find (SaddleRegion.includes point) saddleRegions
  let reverseExtension curves = List.reverseMap Curve2d.reverse curves
  case (regionContaining startPoint, regionContaining endPoint) of
    (Nothing, Nothing) -> Just (Ok segments)
    (Just startRegion, Nothing) ->
      Just Result.do
        extension <- connectingCurves f startRegion firstCurve
        Ok (extension + segments)
    (Nothing, Just endRegion) ->
      Just Result.do
        extension <- connectingCurves f endRegion (Curve2d.reverse lastCurve)
        Ok (segments + reverseExtension extension)
    (Just startRegion, Just endRegion) ->
      Just Result.do
        startExtension <- connectingCurves f startRegion firstCurve
        endExtension <- connectingCurves f endRegion (Curve2d.reverse lastCurve)
        Ok (startExtension + segments + reverseExtension endExtension)

finalizeTangentCurve :: PartialZeros.TangentCurve -> Maybe (NonEmpty (Curve2d Uv.Coordinates), Sign)
finalizeTangentCurve (PartialZeros.DegenerateTangentCurve{}) = Nothing
finalizeTangentCurve (PartialZeros.TangentCurve{segments, sign}) = Just (segments, sign)

finalizeTangentLoop :: PartialZeros.TangentLoop -> (NonEmpty (Curve2d Uv.Coordinates), Sign)
finalizeTangentLoop (PartialZeros.TangentLoop{segments, sign}) = (segments, sign)

finalizeTangentPoint :: PartialZeros.TangentPoint -> (Uv.Point, Sign)
finalizeTangentPoint (PartialZeros.TangentPoint{point, sign}) = (point, sign)

connectingCurves :: Function units -> SaddleRegion -> Curve2d Uv.Coordinates -> Result ZerosError (List (Curve2d Uv.Coordinates))
connectingCurves _ saddleRegion curve = Result.do
  let frame = SaddleRegion.frame saddleRegion
  let localCurve = Curve2d.relativeTo frame curve
  let localCurveFirstDerivative = Curve2d.derivative localCurve
  let localCurveSecondDerivative = VectorCurve2d.derivative localCurveFirstDerivative
  let localStartPoint = Point2d.origin
  let localEndPoint = Curve2d.startPoint localCurve
  let (x0, y0) = Point2d.coordinates localEndPoint
  let localCurveFirstDerivativeAtStart = VectorCurve2d.evaluateAt 0.0 localCurveFirstDerivative
  let localCurveSecondDerivativeAtStart = VectorCurve2d.evaluateAt 0.0 localCurveSecondDerivative
  let k = x0 / Vector2d.xComponent localCurveFirstDerivativeAtStart
  let localEndFirstDerivative = k * localCurveFirstDerivativeAtStart
  let localEndSecondDerivative = k * k * localCurveSecondDerivativeAtStart
  let SaddleRegion.Solution{dydx, d2ydx2} =
        if y0 / x0 > Qty.zero
          then SaddleRegion.positiveSolution saddleRegion
          else SaddleRegion.negativeSolution saddleRegion
  let localStartFirstDerivative = Vector2d.xy x0 (x0 * dydx)
  let localStartSecondDerivative = Vector2d.xy 0.0 (x0 * x0 * d2ydx2)
  let localExtension =
        BezierCurve2d.hermite
          (localStartPoint, [localStartFirstDerivative, localStartSecondDerivative])
          (localEndPoint, [localEndFirstDerivative, localEndSecondDerivative])
  let extension = Curve2d.placeIn frame localExtension
  -- let curveCurvature = Curve2d.curvature' curve
  -- let extensionCurvature = Curve2d.curvature' extension
  -- Debug.log "Extension distance                " (Point2d.distanceFrom localStartPoint localEndPoint)
  -- Debug.log "Error 0.0                         " (evaluateAt (Curve2d.evaluateAt 0.0 extension) f)
  -- Debug.log "Error 0.5                         " (evaluateAt (Curve2d.evaluateAt 0.5 extension) f)
  -- Debug.log "Error 1.0                         " (evaluateAt (Curve2d.evaluateAt 1.0 extension) f)
  -- Debug.log "Extension curvature 0.0           " (Curve1d.evaluateAt 0.0 extensionCurvature)
  -- Debug.log "Extension curvature 0.5           " (Curve1d.evaluateAt 0.5 extensionCurvature)
  -- Debug.log "Extension curvature 1.0           " (Curve1d.evaluateAt 1.0 extensionCurvature)
  -- Debug.log "Curve curvature 0.0               " (Curve1d.evaluateAt 0.0 curveCurvature)
  -- Debug.log "Curve curvature 0.5               " (Curve1d.evaluateAt 0.5 curveCurvature)
  -- Debug.log "Curve curvature 1.0               " (Curve1d.evaluateAt 1.0 curveCurvature)
  -- Debug.log "Extension start derivative        " (VectorCurve2d.evaluateAt 0.0 (Curve2d.derivative extension))
  -- Debug.log "Extension end derivative          " (VectorCurve2d.evaluateAt 1.0 (Curve2d.derivative extension))
  -- Debug.log "Curve start derivative            " (VectorCurve2d.evaluateAt 0.0 (Curve2d.derivative curve) * k)
  -- Debug.log "Extension start second derivative " (VectorCurve2d.evaluateAt 0.0 (VectorCurve2d.derivative (Curve2d.derivative extension)))
  -- Debug.log "Extension end second derivative   " (VectorCurve2d.evaluateAt 1.0 (VectorCurve2d.derivative (Curve2d.derivative extension)))
  -- Debug.log "Curve start second derivative     " (VectorCurve2d.evaluateAt 0.0 (VectorCurve2d.derivative (Curve2d.derivative curve)) * k * k)
  Ok [extension]
