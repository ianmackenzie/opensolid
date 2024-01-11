module Surface1d.Function
  ( Function
  , Interface (..)
  , Solution
  , evaluateAt
  , pointOn
  , segmentBounds
  , boundsOn
  , derivative
  , derivativeIn
  , zero
  , constant
  , parameter
  , solve
  , wrap
  , squared
  , sqrt
  , sin
  , cos
  , curveOnSurface
  , isZero
  , findSolutions
  )
where

import Angle qualified
import Bounds2d (Bounds2d (Bounds2d))
import Bounds2d qualified
import Curve1d (Curve1d (Curve1d))
import Curve1d qualified
import Curve1d.Root qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Debug qualified
import Direction2d qualified
import Float qualified
import Generic qualified
import Int qualified
import Line2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Result qualified
import Surface1d.Solution (Solution)
import Surface1d.Solution qualified as Solution
import Surface1d.Solution.Boundary (Boundary)
import Surface1d.Solution.Boundary qualified as Boundary
import U qualified
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import VectorCurve2d qualified

class (Show function) => Interface function units | function -> units where
  evaluateAtImpl :: Uv.Point -> function -> Qty units
  segmentBoundsImpl :: Uv.Bounds -> function -> Range units
  derivativeImpl :: Parameter -> function -> Function units

data Function units where
  Function ::
    (Interface function units) =>
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
  Product ::
    (Units.Product units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Quotient ::
    (Units.Quotient units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Squared ::
    (Units.Squared units1 units2) =>
    Function units1 ->
    Function units2
  SquareRoot ::
    (Units.Squared units1 units2) =>
    Function units2 ->
    Function units1
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless

deriving instance Show (Function units)

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion
    units1
    units2
    (Function units1')
    (Function units2')

instance Generic.HasZero (Function units) where
  zero = zero

instance Negation (Function units) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product f1 f2) = negate f1 * f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication (Function units) Sign (Function units) where
  function * Positive = function
  function * Negative = -function

instance (units ~ units') => Addition (Function units) (Function units') (Function units) where
  Zero + function = function
  function + Zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance (units ~ units') => Addition (Function units) (Qty units') (Function units) where
  function + value = function + constant value

instance (units ~ units') => Addition (Qty units) (Function units') (Function units) where
  value + function = constant value + function

instance (units ~ units') => Subtraction (Function units) (Function units') (Function units) where
  Zero - function = negate function
  function - Zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance (units ~ units') => Subtraction (Function units) (Qty units') (Function units) where
  function - value = function - constant value

instance (units ~ units') => Subtraction (Qty units) (Function units') (Function units) where
  value - function = constant value - function

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero * _ = Zero
  _ * Zero = Zero
  Constant x * Constant y = Constant (x * y)
  Constant x * function | Units.drop x == 1.0 = Units.add (Units.drop function)
  Constant x * function | Units.drop x == -1.0 = Units.add (Units.drop (negate function))
  Constant x * Negated c = negate x * c
  f1 * (Constant x) = Constant x * f1
  Constant x * Product (Constant y) c =
    Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
  function1 * function2 = Product function1 function2

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function * value = function * constant value

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value * function = constant value * function

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  function / Constant x =
    Units.specialize $
      (Units.generalize 1.0 ./ Units.generalize x) .* Units.generalize function
  function1 / function2 = Quotient function1 function2

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function / value = function / constant value

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value / function = constant value / function

evaluateAt :: Uv.Point -> Function units -> Qty units
evaluateAt uv function =
  case function of
    Function f -> evaluateAtImpl uv f
    Zero -> Qty.zero
    Constant x -> x
    Parameter U -> Point2d.xCoordinate uv
    Parameter V -> Point2d.yCoordinate uv
    Negated f -> negate (evaluateAt uv f)
    Sum f1 f2 -> evaluateAt uv f1 + evaluateAt uv f2
    Difference f1 f2 -> evaluateAt uv f1 - evaluateAt uv f2
    Product f1 f2 -> evaluateAt uv f1 * evaluateAt uv f2
    Quotient f1 f2 -> evaluateAt uv f1 / evaluateAt uv f2
    Squared f -> Qty.squared (evaluateAt uv f)
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
    Parameter U -> Bounds2d.xCoordinate uv
    Parameter V -> Bounds2d.yCoordinate uv
    Negated f -> negate (segmentBounds uv f)
    Sum f1 f2 -> segmentBounds uv f1 + segmentBounds uv f2
    Difference f1 f2 -> segmentBounds uv f1 - segmentBounds uv f2
    Product f1 f2 -> segmentBounds uv f1 * segmentBounds uv f2
    Quotient f1 f2 -> segmentBounds uv f1 / segmentBounds uv f2
    Squared f -> Range.squared (segmentBounds uv f)
    SquareRoot f -> Range.sqrt (segmentBounds uv f)
    Sin f -> Range.sin (segmentBounds uv f)
    Cos f -> Range.cos (segmentBounds uv f)

boundsOn :: Function units -> Uv.Bounds -> Range units
boundsOn function uvBounds = segmentBounds uvBounds function

derivative :: Parameter -> Function units -> Function units
derivative p function =
  case function of
    Function f -> derivativeImpl p f
    Zero -> zero
    Constant _ -> zero
    Parameter p' -> if p == p' then constant 1.0 else zero
    Negated f -> negate (derivative p f)
    Sum f1 f2 -> derivative p f1 + derivative p f2
    Difference f1 f2 -> derivative p f1 - derivative p f2
    Product f1 f2 -> derivative p f1 * f2 + f1 * derivative p f2
    Quotient f1 f2 ->
      let f1' = Units.generalize f1
          f2' = Units.generalize f2
       in Units.specialize ((derivative p f1' .* f2' - f1' .* derivative p f2') ./ squared f2')
    Squared f -> 2.0 * f * derivative p f
    SquareRoot f -> derivative p f / (2.0 * sqrt f)
    Sin f -> cos f * Units.drop (derivative p f)
    Cos f -> negate (sin f) * Units.drop (derivative p f)

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

wrap :: (Interface function units) => function -> Function units
wrap = Function

squared :: (Units.Squared units1 units2) => Function units1 -> Function units2
squared Zero = Zero
squared (Constant x) = Constant (x * x)
squared (Negated f) = squared f
squared (Cos f) = Units.add (cosSquared f)
squared (Sin f) = Units.add (sinSquared f)
squared function = Squared function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2.0 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2.0 * f)

sqrt :: (Units.Squared units1 units2) => Function units2 -> Function units1
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
    (Curve2d.Interface curve Uv.Coordinates) =>
    curve ->
    Function units ->
    CurveOnSurface units

deriving instance Show (CurveOnSurface units)

instance Curve1d.Interface (CurveOnSurface units) units where
  evaluateAtImpl t (CurveOnSurface uvCurve function) =
    evaluateAt (Curve2d.evaluateAtImpl t uvCurve) function

  segmentBoundsImpl t (CurveOnSurface uvCurve function) =
    segmentBounds (Curve2d.segmentBoundsImpl t uvCurve) function

  derivativeImpl (CurveOnSurface uvCurve function) =
    let fU = derivative U function
        fV = derivative V function
        uvT = Curve2d.derivativeImpl uvCurve
        uT = VectorCurve2d.xComponent uvT
        vT = VectorCurve2d.yComponent uvT
     in Curve1d (CurveOnSurface uvCurve fU) * uT + Curve1d (CurveOnSurface uvCurve fV) * vT

curveOnSurface :: Curve2d Uv.Coordinates -> Function units -> Curve1d units
curveOnSurface uvCurve function = Curve1d (CurveOnSurface uvCurve function)

isZero :: (Tolerance units) => Function units -> Bool
isZero function = List.all (~= Qty.zero) (Bounds2d.sample (pointOn function) Uv.domain)

data SolveError
  = ZeroEverywhere
  | HigherOrderIntersection
  | DegenerateCurve
  deriving (Eq, Show, ErrorMessage)

solve :: (Tolerance units) => Function units -> Result SolveError (List Solution)
solve Zero = Error ZeroEverywhere
solve (Constant value) = if value ~= Qty.zero then Error ZeroEverywhere else Ok []
solve f | isZero f = Error ZeroEverywhere
solve f =
  Result.map Pair.first $
    findSolutions derivatives boundaryEdges boundaryPoints Uv.domain U []
 where
  fu = derivative U f
  fv = derivative V f
  fuu = derivative U fu
  fvv = derivative V fv
  fuv = derivative V fu
  derivatives = Derivatives {f, fu, fv, fuu, fvv, fuv}
  (boundaryEdges, boundaryPoints) = findBoundarySolutions f

data Derivatives units = Derivatives
  { f :: Function units
  , fu :: Function units
  , fv :: Function units
  , fuu :: Function units
  , fvv :: Function units
  , fuv :: Function units
  }

findSolutions ::
  (Tolerance units) =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  Uv.Parameter ->
  List Uv.Bounds ->
  Result SolveError (List Solution, List Uv.Bounds)
findSolutions derivatives boundaryEdges boundaryPoints uvBounds bisectionParameter exclusions
  | Range.exclusion Qty.zero fBounds > ?tolerance = Ok ([], []) -- no solutions
  | List.any (Bounds2d.contains uvBounds) exclusions = Ok ([], []) -- no solutions
  | Just result <- generalSolution derivatives uvBounds exclusions = result
  | Just result <- horizontalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions = result
  | Just result <- verticalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions = result
  -- TODO: tangent solutions
  | fBounds ~= Qty.zero && allDerivativesZero uvBounds derivatives =
      Error HigherOrderIntersection
  | otherwise = do
      let (bounds1, bounds2) = Uv.bisect bisectionParameter uvBounds
      let nextBisectionParameter = Uv.cycle bisectionParameter
      (solutions1, exclusions1) <-
        findSolutions derivatives boundaryEdges boundaryPoints bounds1 nextBisectionParameter $
          List.filter (affects bounds1) exclusions
      (solutions2, exclusions2) <-
        findSolutions derivatives boundaryEdges boundaryPoints bounds2 nextBisectionParameter $
          List.filter (affects bounds2) (exclusions1 ++ exclusions)
      return (Solution.merge solutions1 solutions2, exclusions1 ++ exclusions2)
 where
  Derivatives {f} = derivatives
  fBounds = segmentBounds uvBounds f

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
  Line2d.with
    ( Line2d.startPoint startPoint
    , Line2d.direction direction
    , Line2d.length 1.0
    )

findBoundarySolutions :: (Tolerance units) => Function units -> (BoundaryEdges, List BoundaryPoint)
findBoundarySolutions f =
  let p00 = Point2d 0.0 0.0
      p01 = Point2d 0.0 1.0
      p10 = Point2d 1.0 0.0
      leftEdge = boundaryEdge p00 Direction2d.y
      rightEdge = boundaryEdge p10 Direction2d.y
      bottomEdge = boundaryEdge p00 Direction2d.x
      topEdge = boundaryEdge p01 Direction2d.x
      (leftEdgeIsSolution, leftPoints) = edgeSolutions f leftEdge Negative
      (rightEdgeIsSolution, rightPoints) = edgeSolutions f rightEdge Positive
      (bottomEdgeIsSolution, bottomPoints) = edgeSolutions f bottomEdge Negative
      (topEdgeIsSolution, topPoints) = edgeSolutions f topEdge Positive
      boundaryEdges =
        BoundaryEdges
          { leftEdgeIsSolution
          , rightEdgeIsSolution
          , bottomEdgeIsSolution
          , topEdgeIsSolution
          }
      boundaryPoints = List.concat [leftPoints, rightPoints, bottomPoints, topPoints]
   in (boundaryEdges, boundaryPoints)

edgeSolutions :: (Tolerance units) => Function units -> Curve2d Uv.Coordinates -> Sign -> (Bool, List BoundaryPoint)
edgeSolutions f edgeCurve edgeSign =
  case Curve1d.roots (Curve1d (CurveOnSurface edgeCurve f)) of
    Error Curve1d.ZeroEverywhere -> (True, [])
    Ok roots ->
      let toBoundaryPoint root =
            BoundaryPoint
              { point = Curve2d.pointOn edgeCurve (Curve1d.Root.value root)
              , edgeSign
              , rootOrder = Curve1d.Root.order root
              , rootSign = Curve1d.Root.sign root
              }
       in (False, List.map toBoundaryPoint roots)

affects :: Uv.Bounds -> Uv.Bounds -> Bool
affects uvBounds exclusion =
  overlaps exclusion (expandU uvBounds) || overlaps exclusion (expandV uvBounds)

overlaps :: Uv.Bounds -> Uv.Bounds -> Bool
overlaps bounds1 bounds2 = Bounds2d.overlap bounds1 bounds2 > Qty.zero

expand :: Range Unitless -> Range Unitless
expand (Range low high) =
  let halfWidth = 0.5 * (high - low)
   in Range.from (low - halfWidth) (high + halfWidth)

expandU :: Uv.Bounds -> Uv.Bounds
expandU (Bounds2d u v) = Bounds2d (expand u) v

expandV :: Uv.Bounds -> Uv.Bounds
expandV (Bounds2d u v) = Bounds2d u (expand v)

generalSolution ::
  (Tolerance units) =>
  Derivatives units ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (Result SolveError (List Solution, List Uv.Bounds))
generalSolution derivatives uvBounds@(Bounds2d (Range minU maxU) (Range minV maxV)) exclusions
  | List.any (overlaps uvBounds) exclusions = Nothing
  | resolved fuBounds && resolved fvBounds =
      let signAt u v = Qty.sign (evaluateAt (Point2d u v) f)
       in Just $ Result.map (,[]) $ case (signAt minU minV, signAt maxU minV, signAt minU maxV, signAt maxU maxV) of
            (Positive, Positive, Positive, Positive) -> Ok []
            (Negative, Negative, Negative, Negative) -> Ok []
            (Positive, Positive, Negative, Negative) -> rightwardsSolution f fu fv uvBounds
            (Negative, Negative, Positive, Positive) -> leftwardsSolution f fu fv uvBounds
            (Positive, Negative, Positive, Negative) -> downwardsSolution f fu fv uvBounds
            (Negative, Positive, Negative, Positive) -> upwardsSolution f fu fv uvBounds
            -- One positive corner
            (Positive, Negative, Negative, Negative) ->
              -- Bottom left positive
              let startV = solveVertically f minU maxV minV
                  endU = solveHorizontally f maxU minU minV
                  curveBounds = Bounds2d (Range.from minU endU) (Range.from minV startV)
                  startBoundary = Boundary.left curveBounds
                  endBoundary = Boundary.bottom curveBounds
               in if endU - minU >= startV - minV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv minU endU startV minV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv endU minU startV minV)
            (Negative, Positive, Negative, Negative) ->
              -- Bottom right positive
              let startU = solveHorizontally f minU maxU minV
                  endV = solveVertically f maxU maxV minV
                  curveBounds = Bounds2d (Range.from startU maxU) (Range.from minV endV)
                  startBoundary = Boundary.bottom curveBounds
                  endBoundary = Boundary.right curveBounds
               in if maxU - startU >= endV - minV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv startU maxU endV minV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv startU maxU minV endV)
            (Negative, Negative, Positive, Negative) ->
              -- Top left positive
              let startU = solveHorizontally f maxU minU maxV
                  endV = solveVertically f minU minV maxV
                  curveBounds = Bounds2d (Range.from minU startU) (Range.from endV maxV)
                  startBoundary = Boundary.top curveBounds
                  endBoundary = Boundary.left curveBounds
               in if startU - minU >= maxV - endV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv startU minU endV maxV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv startU minU maxV endV)
            (Negative, Negative, Negative, Positive) ->
              -- Top right positive
              let startV = solveVertically f maxU minV maxV
                  endU = solveHorizontally f minU maxU maxV
                  curveBounds = Bounds2d (Range.from endU maxU) (Range.from startV maxV)
                  startBoundary = Boundary.right curveBounds
                  endBoundary = Boundary.top curveBounds
               in if maxU - endU >= maxV - startV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv maxU endU startV maxV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv endU maxU startV maxV)
            -- One negative corner
            (Negative, Positive, Positive, Positive) ->
              -- Bottom left negative
              let endV = solveVertically f minU minV maxV
                  startU = solveHorizontally f minU maxU minV
                  curveBounds = Bounds2d (Range.from minU startU) (Range.from minV endV)
                  startBoundary = Boundary.bottom curveBounds
                  endBoundary = Boundary.left curveBounds
               in if startU - minU >= endV - minV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv startU minU minV endV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv minU startU minV endV)
            (Positive, Negative, Positive, Positive) ->
              -- Bottom right negative
              let startV = solveVertically f maxU minV maxV
                  endU = solveHorizontally f maxU minU minV
                  curveBounds = Bounds2d (Range.from endU maxU) (Range.from minV startV)
                  startBoundary = Boundary.right curveBounds
                  endBoundary = Boundary.bottom curveBounds
               in if maxU - endU >= startV - minV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv maxU endU minV startV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv maxU endU startV minV)
            (Positive, Positive, Negative, Positive) ->
              -- Top left negative
              let startV = solveVertically f minU maxV minV
                  endU = solveHorizontally f minU maxU maxV
                  curveBounds = Bounds2d (Range.from minU endU) (Range.from startV maxV)
                  startBoundary = Boundary.left curveBounds
                  endBoundary = Boundary.top curveBounds
               in if endU - minU >= maxV - startV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv minU endU maxV startV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv minU endU startV maxV)
            (Positive, Positive, Positive, Negative) ->
              -- Top right negative
              let startU = solveHorizontally f maxU minU maxV
                  endV = solveVertically f maxU maxV minV
                  curveBounds = Bounds2d (Range.from startU maxU) (Range.from endV maxV)
                  startBoundary = Boundary.top curveBounds
                  endBoundary = Boundary.right curveBounds
               in if maxU - startU >= maxV - endV
                    then crossingSolution startBoundary endBoundary (horizontalCurve f fu fv startU maxU maxV endV)
                    else crossingSolution startBoundary endBoundary (verticalCurve f fu fv maxU startU maxV endV)
            -- Shouldn't happen
            (Negative, Positive, Positive, Negative) -> internalError "Inconsistent derivatives"
            (Positive, Negative, Negative, Positive) -> internalError "Inconsistent derivatives"
  | otherwise = Nothing
 where
  Derivatives {f, fu, fv} = derivatives
  fuBounds = segmentBounds uvBounds fu
  fvBounds = segmentBounds uvBounds fv

resolved :: Range units -> Bool
resolved range = Qty.abs (Range.resolution range) >= 0.5

isStrictlyInside :: Uv.Bounds -> BoundaryPoint -> Bool
isStrictlyInside bounds (BoundaryPoint {point}) = Bounds2d.inclusion point bounds > 0.0

horizontalSolution ::
  (Tolerance units) =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (Result SolveError (List Solution, List Uv.Bounds))
horizontalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions
  | List.any (overlaps expandedBounds) exclusions =
      Nothing
  | Qty.abs fvResolution >= 0.5
  , (bottomEdgeIsSolution && minV == 0.0) || (topEdgeIsSolution && maxV == 1.0) =
      Just (Ok ([], []))
  | fvResolution >= 0.5
  , bottomSign == Resolved Negative
  , topSign == Resolved Positive
  , Just solutionBounds <- trimmedBounds =
      Just (Result.map (,newExclusions) (leftwardsSolution f fu fv solutionBounds))
  | fvResolution <= -0.5
  , bottomSign == Resolved Positive
  , topSign == Resolved Negative
  , Just solutionBounds <- trimmedBounds =
      Just (Result.map (,newExclusions) (rightwardsSolution f fu fv solutionBounds))
  | otherwise =
      Nothing
 where
  Bounds2d uRange vRange = uvBounds
  Range minU maxU = uRange
  Range minV maxV = vRange
  Derivatives {f, fu, fv} = derivatives
  BoundaryEdges {bottomEdgeIsSolution, topEdgeIsSolution} = boundaryEdges

  vHalfWidth = 0.5 * Range.width vRange
  vBottom = minV - vHalfWidth
  vTop = maxV + vHalfWidth
  expandedVRange = Range.from vBottom vTop
  expandedBounds = Bounds2d uRange expandedVRange

  fvResolution = Range.resolution (segmentBounds expandedBounds fv)
  sliceSign v uSubRange = sign (boundsOn f (Bounds2d uSubRange (Range.constant v)))
  bottomSign = Range.resolve (sliceSign vBottom) uRange
  topSign = Range.resolve (sliceSign vTop) uRange

  newExclusions = [Bounds2d uRange (Range.from vBottom minV), Bounds2d uRange (Range.from maxV vTop)]
  trimmedBounds =
    case List.filter (isStrictlyInside expandedBounds) boundaryPoints of
      [] -> Just expandedBounds
      List.One (BoundaryPoint {point = Point2d u0 _, edgeSign, rootOrder, rootSign}) ->
        case (rootOrder, edgeSign, rootSign * Qty.sign fvResolution) of
          (Int.Even, Negative, Negative) -> Just (Bounds2d (Range.from u0 maxU) (Range.from minV vTop))
          (Int.Even, Negative, Positive) -> Just (Bounds2d (Range.from minU u0) (Range.from minV vTop))
          (Int.Even, Positive, Negative) -> Just (Bounds2d (Range.from minU u0) (Range.from vBottom maxV))
          (Int.Even, Positive, Positive) -> Just (Bounds2d (Range.from u0 maxU) (Range.from vBottom maxV))
          (Int.Odd, Negative, Negative) -> Just (Bounds2d (Range.from minU maxU) (Range.from minV vTop))
          (Int.Odd, Negative, Positive) -> Nothing
          (Int.Odd, Positive, Negative) -> Nothing
          (Int.Odd, Positive, Positive) -> Just (Bounds2d (Range.from minU maxU) (Range.from vBottom maxV))
      List.TwoOrMore -> Nothing

verticalSolution ::
  (Tolerance units) =>
  Derivatives units ->
  BoundaryEdges ->
  List BoundaryPoint ->
  Uv.Bounds ->
  List Uv.Bounds ->
  Maybe (Result SolveError (List Solution, List Uv.Bounds))
verticalSolution derivatives boundaryEdges boundaryPoints uvBounds exclusions
  | List.any (overlaps expandedBounds) exclusions =
      Nothing
  | Qty.abs fuResolution >= 0.5
  , (leftEdgeIsSolution && minU == 0.0) || (rightEdgeIsSolution && maxU == 1.0) =
      Just (Ok ([], []))
  | fuResolution >= 0.5
  , leftSign == Resolved Negative
  , rightSign == Resolved Positive
  , Just solutionBounds <- trimmedBounds =
      Just (Result.map (,newExclusions) (upwardsSolution f fu fv solutionBounds))
  | fuResolution <= -0.5
  , leftSign == Resolved Positive
  , rightSign == Resolved Negative
  , Just solutionBounds <- trimmedBounds =
      Just (Result.map (,newExclusions) (downwardsSolution f fu fv solutionBounds))
  | otherwise =
      Nothing
 where
  Bounds2d uRange vRange = uvBounds
  Range minU maxU = uRange
  Range minV maxV = vRange
  Derivatives {f, fu, fv} = derivatives
  BoundaryEdges {leftEdgeIsSolution, rightEdgeIsSolution} = boundaryEdges

  uHalfWidth = 0.5 * Range.width uRange
  uLeft = minU - uHalfWidth
  uRight = maxU + uHalfWidth
  expandedURange = Range.from uLeft uRight
  expandedBounds = Bounds2d expandedURange vRange

  fuResolution = Range.resolution (segmentBounds expandedBounds fu)
  sliceSign u vSubRange = sign (boundsOn f (Bounds2d (Range.constant u) vSubRange))
  leftSign = Range.resolve (sliceSign uLeft) vRange
  rightSign = Range.resolve (sliceSign uRight) vRange

  newExclusions = [Bounds2d (Range.from uLeft minU) vRange, Bounds2d (Range.from maxU uRight) vRange]
  trimmedBounds =
    case List.filter (isStrictlyInside expandedBounds) boundaryPoints of
      [] -> Just expandedBounds
      List.One (BoundaryPoint {point = Point2d _ v0, edgeSign, rootOrder, rootSign}) ->
        case (rootOrder, edgeSign, rootSign * Qty.sign fuResolution) of
          (Int.Even, Negative, Negative) -> Just (Bounds2d (Range.from minU uRight) (Range.from v0 maxV))
          (Int.Even, Negative, Positive) -> Just (Bounds2d (Range.from minU uRight) (Range.from minV v0))
          (Int.Even, Positive, Negative) -> Just (Bounds2d (Range.from uLeft maxU) (Range.from minV v0))
          (Int.Even, Positive, Positive) -> Just (Bounds2d (Range.from uLeft maxU) (Range.from v0 maxV))
          (Int.Odd, Negative, Negative) -> Just (Bounds2d (Range.from minU uRight) (Range.from minV maxV))
          (Int.Odd, Negative, Positive) -> Nothing
          (Int.Odd, Positive, Negative) -> Nothing
          (Int.Odd, Positive, Positive) -> Just (Bounds2d (Range.from uLeft maxU) (Range.from minV maxV))
      List.TwoOrMore -> Nothing

rightwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  Result SolveError (List Solution)
rightwardsSolution f fu fv (Bounds2d (Range minU maxU) vRange@(Range minV maxV)) =
  crossingSolution (Boundary.Left minU vRange) (Boundary.Right maxU vRange) $
    horizontalCurve f fu fv minU maxU maxV minV

leftwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  Result SolveError (List Solution)
leftwardsSolution f fu fv (Bounds2d (Range minU maxU) vRange@(Range minV maxV)) =
  crossingSolution (Boundary.Right maxU vRange) (Boundary.Left minU vRange) $
    horizontalCurve f fu fv maxU minU minV maxV

upwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  Result SolveError (List Solution)
upwardsSolution f fu fv (Bounds2d uRange@(Range minU maxU) (Range minV maxV)) =
  crossingSolution (Boundary.Bottom uRange minV) (Boundary.Top uRange maxV) $
    verticalCurve f fu fv minU maxU minV maxV

downwardsSolution ::
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  Result SolveError (List Solution)
downwardsSolution f fu fv (Bounds2d uRange@(Range minU maxU) (Range minV maxV)) =
  crossingSolution (Boundary.Top uRange maxV) (Boundary.Bottom uRange minV) $
    verticalCurve f fu fv maxU minU maxV minV

crossingSolution ::
  Boundary ->
  Boundary ->
  Result SolveError (Curve2d Uv.Coordinates) ->
  Result SolveError (List Solution)
crossingSolution start end =
  Result.map (\curve -> [Solution.CrossingCurve {start, end, segments = NonEmpty.singleton curve}])

horizontalCurve ::
  Function units ->
  Function units ->
  Function units ->
  Float ->
  Float ->
  Float ->
  Float ->
  Result SolveError (Curve2d Uv.Coordinates)
horizontalCurve f fu fv uStart uEnd vLow vHigh =
  exactly (Curve2d.from (HorizontalCurve f (-fu / fv) uStart uEnd vLow vHigh))
    |> Result.mapError (\Curve2d.DegenerateCurve -> DegenerateCurve)
    -- Sanity check that we don't attempt to evaluate outside the overall UV domain
    |> Debug.assert (uStart >= 0.0)
    |> Debug.assert (uEnd <= 1.0)
    |> Debug.assert (vLow >= 0.0)
    |> Debug.assert (vHigh <= 1.0)

verticalCurve ::
  Function units ->
  Function units ->
  Function units ->
  Float ->
  Float ->
  Float ->
  Float ->
  Result SolveError (Curve2d Uv.Coordinates)
verticalCurve f fu fv uLow uHigh vStart vEnd =
  exactly (Curve2d.from (VerticalCurve f (-fv / fu) uLow uHigh vStart vEnd))
    |> Result.mapError (\Curve2d.DegenerateCurve -> DegenerateCurve)
    -- Sanity check that we don't attempt to evaluate outside the overall UV domain
    |> Debug.assert (uLow >= 0.0)
    |> Debug.assert (uHigh <= 1.0)
    |> Debug.assert (vStart >= 0.0)
    |> Debug.assert (vEnd <= 1.0)

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
  (Tolerance units) =>
  Uv.Bounds ->
  Derivatives units ->
  Bool
allDerivativesZero uvBounds (Derivatives {fu, fv, fuu, fvv, fuv}) =
  segmentBounds uvBounds fu ~= Qty.zero
    && segmentBounds uvBounds fv ~= Qty.zero
    && segmentBounds uvBounds fuu ~= Qty.zero
    && segmentBounds uvBounds fvv ~= Qty.zero
    && segmentBounds uvBounds fuv ~= Qty.zero

-- TODO: have the tolerance here be much larger
-- (based on the derivative resolution)
-- to avoid expensive bisection near zeros
sign :: (Tolerance units) => Range units -> Fuzzy Sign
sign range
  | Range.minValue range > ?tolerance = Resolved Positive
  | Range.maxValue range < negate ?tolerance = Resolved Negative
  | otherwise = Unresolved

data HorizontalCurve units
  = HorizontalCurve
      (Function units) -- f
      (Function Unitless) -- dv/du
      Float -- uStart
      Float -- uEnd
      Float -- vLow
      Float -- vHigh
  deriving (Show)

instance Curve2d.Interface (HorizontalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t (HorizontalCurve f _ uStart uEnd vLow vHigh) =
    let u = Float.interpolateFrom uStart uEnd t
        v = solveVertically f u vLow vHigh
     in Point2d u v

  segmentBoundsImpl (Range t1 t2) (HorizontalCurve f vu uStart uEnd vLow vHigh) =
    let u1 = Float.interpolateFrom uStart uEnd t1
        u2 = Float.interpolateFrom uStart uEnd t2
        v1 = solveVertically f u1 vLow vHigh
        v2 = solveVertically f u2 vLow vHigh
        slopeBounds = segmentBounds (Bounds2d (Range.from u1 u2) (Range.from vLow vHigh)) vu
        vRange = parallelogramBounds u1 u2 v1 v2 slopeBounds
     in Bounds2d (Range.from u1 u2) vRange

  derivativeImpl crossingCurve@(HorizontalCurve _ vu uStart uEnd _ _) =
    let deltaU = uEnd - uStart
        uT = Curve1d.constant deltaU
        vT = deltaU * Curve1d (CurveOnSurface crossingCurve vu)
     in VectorCurve2d.xy uT vT

  reverseImpl (HorizontalCurve f vu uStart uEnd vLow vHigh) =
    HorizontalCurve f vu uEnd uStart vLow vHigh

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl U.domain crossingCurve

data VerticalCurve units
  = VerticalCurve
      (Function units) -- f
      (Function Unitless) -- du/dv
      Float -- uLow
      Float -- uHigh
      Float -- vStart
      Float -- vEnd
  deriving (Show)

instance Curve2d.Interface (VerticalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t (VerticalCurve f _ uLow uHigh vStart vEnd) =
    let v = Float.interpolateFrom vStart vEnd t
        u = solveHorizontally f uLow uHigh v
     in Point2d u v

  segmentBoundsImpl (Range t1 t2) (VerticalCurve f uv uLow uHigh vStart vEnd) =
    let v1 = Float.interpolateFrom vStart vEnd t1
        v2 = Float.interpolateFrom vStart vEnd t2
        u1 = solveHorizontally f uLow uHigh v1
        u2 = solveHorizontally f uLow uHigh v2
        slopeBounds = segmentBounds (Bounds2d (Range.from uLow uHigh) (Range.from v1 v2)) uv
        uRange = parallelogramBounds v1 v2 u1 u2 slopeBounds
     in Bounds2d uRange (Range.from v1 v2)

  derivativeImpl crossingCurve@(VerticalCurve _ uv _ _ vStart vEnd) =
    let deltaV = vEnd - vStart
        vT = Curve1d.constant deltaV
        uT = deltaV * Curve1d (CurveOnSurface crossingCurve uv)
     in VectorCurve2d.xy uT vT

  reverseImpl (VerticalCurve f uv uLow uHigh vStart vEnd) =
    VerticalCurve f uv uLow uHigh vEnd vStart

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl U.domain crossingCurve

solveVertically :: Function units -> Float -> Float -> Float -> Float
solveVertically f u v1 v2
  | valueAt v1 >= Qty.zero = v1
  | valueAt v2 <= Qty.zero = v2
  | otherwise = bisect v1 v2
 where
  valueAt v = evaluateAt (Point2d u v) f
  bisect vLow vHigh
    | vMid == vLow || vMid == vHigh = vMid
    | fMid < Qty.zero = bisect vMid vHigh
    | fMid > Qty.zero = bisect vLow vMid
    | otherwise = vMid
   where
    vMid = Qty.midpoint vLow vHigh
    fMid = valueAt vMid

solveHorizontally :: Function units -> Float -> Float -> Float -> Float
solveHorizontally f u1 u2 v
  | valueAt u1 >= Qty.zero = u1
  | valueAt u2 <= Qty.zero = u2
  | otherwise = bisect u1 u2
 where
  valueAt u = evaluateAt (Point2d u v) f
  bisect uLow uHigh
    | uMid == uLow || uMid == uHigh = uMid
    | fMid < Qty.zero = bisect uMid uHigh
    | fMid > Qty.zero = bisect uLow uMid
    | otherwise = uMid
   where
    uMid = Qty.midpoint uLow uHigh
    fMid = valueAt uMid

parallelogramBounds ::
  Float ->
  Float ->
  Float ->
  Float ->
  Range Unitless ->
  Range Unitless
parallelogramBounds x1 x2 y1 y2 (Range minSlope maxSlope) =
  let deltaX = x2 - x1
      deltaY = y2 - y1
      deltaXLow = (maxSlope * deltaX - deltaY) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
      deltaXHigh = (deltaY - minSlope * deltaX) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
      yLow = y1 + minSlope * deltaXLow
      yHigh = y1 + maxSlope * deltaXHigh
   in Range.hull4 y1 y2 yLow yHigh
