{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Function
  ( Function (Constant)
  , Interface (..)
  , evaluate
  , bounds
  , derivative
  , derivativeIn
  , zero
  , constant
  , parameter
  , zeros
  , wrap
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , curveOnSurface
  , isZero
  )
where

import Angle qualified
import Axis2d qualified
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Result qualified
import Solve1d qualified
import Solve2d (Subdomain)
import Solve2d qualified
import Surface1d.Function.PartialZeros (PartialZeros)
import Surface1d.Function.PartialZeros qualified as PartialZeros
import Surface1d.Function.SaddleRegion (SaddleRegion (..))
import Surface1d.Function.Zeros (Zeros (..))
import Surface1d.Function.Zeros qualified as Zeros
import Tolerance qualified
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Vector2d qualified
import VectorCurve2d qualified

class Show function => Interface function units | function -> units where
  evaluateImpl :: function -> Uv.Point -> Qty units
  boundsImpl :: function -> Uv.Bounds -> Range units
  derivativeImpl :: Parameter -> function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
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
  SquareRoot' ::
    Function (units :*: units) ->
    Function units
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
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Negation (Function units) where
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
  Constant (Qty 0.0) + function = function
  function + Constant (Qty 0.0) = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units_ => Addition (Function units) (Qty units_) (Function units) where
  function + value = function + constant value

instance units ~ units_ => Addition (Qty units) (Function units_) (Function units) where
  value + function = constant value + function

instance units ~ units_ => Subtraction (Function units) (Function units_) (Function units) where
  Constant (Qty 0.0) - function = negate function
  function - Constant (Qty 0.0) = function
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
  Constant (Qty 0.0) .*. _ = zero
  _ .*. Constant (Qty 0.0) = zero
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
  function .*. value = function .*. Float.int value

instance Multiplication Int (Function units) (Function units)

instance Multiplication' Int (Function units) where
  type Int .*. Function units = Function (Unitless :*: units)
  value .*. function = Float.int value .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Division' (Function units1) (Function units2) where
  type Function units1 ./. Function units2 = Function (units1 :/: units2)
  Constant (Qty 0.0) ./. _ = zero
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
  function ./. value = function ./. Float.int value

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
  value ./. function = Float.int value ./. function

evaluate :: Function units -> Uv.Point -> Qty units
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Constant x -> x
  Coerce f -> Units.coerce (evaluate f uv)
  Parameter U -> Point2d.xCoordinate uv
  Parameter V -> Point2d.yCoordinate uv
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product' f1 f2 -> evaluate f1 uv .*. evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. evaluate f2 uv
  Squared' f -> Qty.squared' (evaluate f uv)
  SquareRoot' f -> Qty.sqrt' (evaluate f uv)
  Sin f -> Angle.sin (evaluate f uv)
  Cos f -> Angle.cos (evaluate f uv)

bounds :: Function units -> Uv.Bounds -> Range units
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Constant x -> Range.constant x
  Coerce f -> Units.coerce (bounds f uv)
  Parameter U -> Bounds2d.xCoordinate uv
  Parameter V -> Bounds2d.yCoordinate uv
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product' f1 f2 -> bounds f1 uv .*. bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. bounds f2 uv
  Squared' f -> Range.squared' (bounds f uv)
  SquareRoot' f -> Range.sqrt' (bounds f uv)
  Sin f -> Range.sin (bounds f uv)
  Cos f -> Range.cos (bounds f uv)

derivative :: Parameter -> Function units -> Function units
derivative varyingParameter function =
  case function of
    Function f -> derivativeImpl varyingParameter f
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
    SquareRoot' f -> derivative varyingParameter f .!/! (2 * sqrt' f)
    Sin f -> cos f * Angle.unitless (derivative varyingParameter f)
    Cos f -> negate (sin f) * Angle.unitless (derivative varyingParameter f)

derivativeIn :: Uv.Direction -> Function units -> Function units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: Function units
zero = constant Qty.zero

constant :: Qty units -> Function units
constant = Constant

parameter :: Parameter -> Function Unitless
parameter = Parameter

wrap :: Interface function units => function -> Function units
wrap = Function

squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared function = Units.specialize (squared' function)

squared' :: Function units -> Function (units :*: units)
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
sqrt function = sqrt' (Units.unspecialize function)

sqrt' :: Function (units :*: units) -> Function units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' function = SquareRoot' function

sin :: Function Radians -> Function Unitless
sin (Constant x) = constant (Angle.sin x)
sin function = Sin function

cos :: Function Radians -> Function Unitless
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
  pointOnImpl (CurveOnSurface uvCurve function) t =
    evaluate function (Curve2d.evaluateAtImpl t uvCurve)

  segmentBoundsImpl (CurveOnSurface uvCurve function) t =
    bounds function (Curve2d.segmentBoundsImpl t uvCurve)

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
isZero function = List.all (~= Qty.zero) (Bounds2d.sample (evaluate function) Uv.domain)

data ZerosError
  = ZeroEverywhere
  | HigherOrderZero
  deriving (Eq, Show, Error)

zeros :: Tolerance units => Function units -> Result ZerosError Zeros
zeros (Constant value) = if value ~= Qty.zero then Error ZeroEverywhere else Ok Zeros.empty
zeros function | isZero function = Error ZeroEverywhere
zeros function = Result.do
  let derivatives = computeDerivatives function
  let cache = Solve2d.init (computeDerivativeBounds derivatives)
  tangentSolutions <-
    Solve2d.search (findTangentSolution derivatives) cache []
      ?? Error HigherOrderZero
  solutions <-
    Solve2d.search (findCrossingSolution derivatives) cache tangentSolutions
      ?? Error HigherOrderZero
  let partialZeros = List.foldl addSolution PartialZeros.empty solutions
  Ok (PartialZeros.finalize partialZeros)

data Solution
  = CrossingCurveSolution PartialZeros.CrossingCurve
  | TangentPointSolution (Uv.Point, Sign)
  | SaddleRegionSolution SaddleRegion
  deriving (Show)

addSolution :: PartialZeros -> (Solution, Subdomain) -> PartialZeros
addSolution partialZeros (solution, subdomain) = case solution of
  CrossingCurveSolution curve -> PartialZeros.addCrossingCurve curve partialZeros
  TangentPointSolution tangentPoint -> PartialZeros.addTangentPoint tangentPoint partialZeros
  SaddleRegionSolution saddleRegion -> PartialZeros.addSaddleRegion subdomain saddleRegion partialZeros

data Derivatives units = Derivatives
  { f :: Function units
  , fu :: Function units
  , fv :: Function units
  , fuu :: Function units
  , fuv :: Function units
  , fvv :: Function units
  }
  deriving (Show)

computeDerivatives :: Function units -> Derivatives units
computeDerivatives f = do
  let fu = derivative U f
  let fv = derivative V f
  let fuu = derivative U fu
  let fuv = derivative U fv
  let fvv = derivative V fv
  Derivatives{f, fu, fv, fuu, fuv, fvv}

data DerivativeBounds units = DerivativeBounds
  { fBounds :: ~(Range units)
  , fuBounds :: ~(Range units)
  , fvBounds :: ~(Range units)
  , fuuBounds :: ~(Range units)
  , fuvBounds :: ~(Range units)
  , fvvBounds :: ~(Range units)
  }

computeDerivativeBounds :: Derivatives units -> Uv.Bounds -> DerivativeBounds units
computeDerivativeBounds (Derivatives{f, fu, fv, fuu, fvv, fuv}) uvBounds =
  DerivativeBounds
    { fBounds = bounds f uvBounds
    , fuBounds = bounds fu uvBounds
    , fvBounds = bounds fv uvBounds
    , fuuBounds = bounds fuu uvBounds
    , fuvBounds = bounds fuv uvBounds
    , fvvBounds = bounds fvv uvBounds
    }

findTangentSolution ::
  Tolerance units =>
  Derivatives units ->
  Subdomain ->
  DerivativeBounds units ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions Solution
findTangentSolution derivatives subdomain derivativeBounds exclusions = do
  let DerivativeBounds{fBounds, fuBounds, fvBounds} = derivativeBounds
  if
    -- The function is non-zero for this subdomain, so no solutions
    | not (fBounds ^ Qty.zero) -> Solve2d.pass
    -- Derivative with respect to U is non-zero for this subdomain, so no tangent solutions
    | not (fuBounds ^ Qty.zero) -> Solve2d.pass
    -- Derivative with respect to V is non-zero for this subdomain, so no tangent solutions
    | not (fvBounds ^ Qty.zero) -> Solve2d.pass
    | otherwise -> case exclusions of
        Solve2d.NoExclusions
          -- Try to find a tangent point (saddle or otherwise)
          | Just solution <- tangentPointSolution derivatives subdomain derivativeBounds ->
              Solve2d.return solution
          -- TODO try to find tangent curve
          | otherwise -> Solve2d.recurse
        Solve2d.SomeExclusions _ -> Solve2d.recurse

tangentPointSolution ::
  Tolerance units =>
  Derivatives units ->
  Subdomain ->
  DerivativeBounds units ->
  Maybe Solution
tangentPointSolution derivatives subdomain derivativeBounds = do
  let DerivativeBounds{fuuBounds, fuvBounds, fvvBounds} = derivativeBounds
  let determinantResolution = Range.resolution (fuuBounds .*. fvvBounds - fuvBounds .*. fuvBounds)
  if Qty.abs determinantResolution < 0.5
    then Nothing
    else do
      let Derivatives{f, fu, fv, fuu, fuv, fvv} = derivatives
      let maybePoint =
            Solve2d.unique
              (bounds fu)
              (evaluate fu)
              (evaluate fuu)
              (evaluate fuv)
              (bounds fv)
              (evaluate fv)
              (evaluate fuv)
              (evaluate fvv)
              (Solve2d.interior subdomain)
      if
        | Just point <- maybePoint
        , evaluate f point ~= Qty.zero ->
            -- We've found a tangent point! Now to check if it's a saddle point
            case Qty.sign determinantResolution of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Qty.sign (Range.minValue fuuBounds)
                Just (TangentPointSolution (point, sign))
              Negative ->
                Just (saddleRegionSolution derivatives point)
        | otherwise -> Nothing

saddleRegionSolution :: Tolerance units => Derivatives units -> Uv.Point -> Solution
saddleRegionSolution derivatives point = do
  let Derivatives{f, fuu, fuv, fvv} = derivatives
  let fuuValue = evaluate fuu point
  let fuvValue = evaluate fuv point
  let fvvValue = evaluate fvv point
  let sqrtD = Qty.sqrt' (fuvValue .*. fuvValue - fuuValue .*. fvvValue)
  let (d1, d2) =
        if Qty.abs fuuValue >= Qty.abs fvvValue
          then
            ( Vector2d.normalize (Vector2d.xy (-fuvValue + sqrtD) fuuValue)
            , Vector2d.normalize (Vector2d.xy (-fuvValue - sqrtD) fuuValue)
            )
          else
            ( Vector2d.normalize (Vector2d.xy fvvValue (-fuvValue + sqrtD))
            , Vector2d.normalize (Vector2d.xy fvvValue (-fuvValue - sqrtD))
            )
  let xDirection = Direction2d.unsafe (Vector2d.normalize (d1 + d2))
  let xAxis = Axis2d.through point xDirection
  let frame = Frame2d.fromXAxis xAxis
  let fXY = reparameterize frame f
  let fx = derivative U fXY
  let fy = derivative V fXY
  let fxx = derivative U fx
  let fxy = derivative V fx
  let fyy = derivative V fy
  let fxxx = derivative U fxx
  let fxxy = derivative V fxx
  let fxyy = derivative U fyy
  let fyyy = derivative V fyy
  let derivativesXY = Derivatives{f = fXY, fu = fx, fv = fy, fuu = fxx, fuv = fxy, fvv = fyy}
  let fxxValue = evaluate fxx Point2d.origin
  let fyyValue = evaluate fyy Point2d.origin
  let fxxxValue = evaluate fxxx Point2d.origin
  let fxxyValue = evaluate fxxy Point2d.origin
  let fxyyValue = evaluate fxyy Point2d.origin
  let fyyyValue = evaluate fyyy Point2d.origin
  let positiveA = Qty.sqrt' (-fxxValue .*. fyyValue) / Qty.abs fyyValue
  let negativeA = -positiveA
  let b a = -(fyyyValue * a ** 3 + 3 * fxyyValue * a ** 2 + 3 * fxxyValue * a + fxxxValue) / (3 * a * fyyValue)
  let connectingCurves endpoint = do
        let (xEnd, yEnd) = Point2d.coordinates (Point2d.relativeTo frame endpoint)
        let connectorSize = 1e-3
        let xConnection = if Qty.abs xEnd <= connectorSize then xEnd else connectorSize * Qty.sign xEnd
        let yRange =
              case Qty.sign fxxValue of
                Negative -> Range.from 0.0 yEnd
                Positive -> Range.from yEnd 0.0
        let exactCurveXY = horizontalCurve derivativesXY xConnection xEnd yRange True
        let degenerateCurveXY = horizontalCurve derivativesXY 0.0 xConnection yRange True
        let dydx = case Qty.sign (yEnd / xEnd) of
              Positive -> positiveA
              Negative -> negativeA
        let d2ydx2 = b dydx
        let startFirstDerivativeXY = Vector2d.xy xConnection (xConnection * dydx)
        let startSecondDerivativeXY = Vector2d.xy 0.0 (xConnection * xConnection * d2ydx2)
        let startCondition = (Point2d.origin, [startFirstDerivativeXY, startSecondDerivativeXY])
        let interpolatedCurveXY = Curve2d.removeStartDegeneracy 2 startCondition degenerateCurveXY
        let exactCurve = Curve2d.placeIn frame exactCurveXY
        let interpolatedCurve = Curve2d.placeIn frame interpolatedCurveXY
        -- let exactCurvature = let ?tolerance = 1e-9 in Curve2d.curvature' exactCurve |> Result.withDefault Curve1d.zero
        -- let interpolatedCurvature = let ?tolerance = 1e-9 in Curve2d.curvature' interpolatedCurve |> Result.withDefault Curve1d.zero
        -- Debug.log "Extension distance                " (Point2d.distanceFrom localStartPoint localEndPoint)
        -- Debug.log "Error 0.0                         " (evaluateAt (Curve2d.evaluateAt 0.0 interpolatedCurve) f)
        -- Debug.log "Error 0.5                         " (evaluateAt (Curve2d.evaluateAt 0.5 interpolatedCurve) f)
        -- Debug.log "Error 1.0                         " (evaluateAt (Curve2d.evaluateAt 1.0 interpolatedCurve) f)
        -- Debug.log "Interpolated curvature 0.0           " (Curve1d.evaluateAt 0.0 interpolatedCurvature)
        -- Debug.log "Interpolated curvature 0.5           " (Curve1d.evaluateAt 0.5 interpolatedCurvature)
        -- Debug.log "Interpolated curvature 1.0           " (Curve1d.evaluateAt 1.0 interpolatedCurvature)
        -- Debug.log "Exact curvature 0.0                  " (Curve1d.evaluateAt 0.0 exactCurvature)
        -- Debug.log "Exact curvature 0.5                  " (Curve1d.evaluateAt 0.5 exactCurvature)
        -- Debug.log "Exact curvature 1.0                  " (Curve1d.evaluateAt 1.0 exactCurvature)
        -- Debug.log "Extension start derivative        " (VectorCurve2d.evaluateAt 0.0 (Curve2d.derivative extension))
        -- Debug.log "Extension end derivative          " (VectorCurve2d.evaluateAt 1.0 (Curve2d.derivative extension))
        -- Debug.log "Curve start derivative            " (VectorCurve2d.evaluateAt 0.0 (Curve2d.derivative curve) * k)
        -- Debug.log "Extension start second derivative " (VectorCurve2d.evaluateAt 0.0 (VectorCurve2d.derivative (Curve2d.derivative extension)))
        -- Debug.log "Extension end second derivative   " (VectorCurve2d.evaluateAt 1.0 (VectorCurve2d.derivative (Curve2d.derivative extension)))
        -- Debug.log "Curve start second derivative     " (VectorCurve2d.evaluateAt 0.0 (VectorCurve2d.derivative (Curve2d.derivative curve)) * k * k)
        if xConnection == xEnd then [interpolatedCurve] else [interpolatedCurve, exactCurve]
  SaddleRegionSolution (SaddleRegion{point, connectingCurves = connectingCurves})

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
  evaluateImpl (Reparameterized frame function) uvPoint =
    evaluate function (Point2d.placeIn frame uvPoint)

  boundsImpl (Reparameterized frame function) uvBounds =
    bounds function (Bounds2d.placeIn frame uvBounds)

  derivativeImpl U (Reparameterized frame function) =
    reparameterize frame (derivativeIn (Frame2d.xDirection frame) function)
  derivativeImpl V (Reparameterized frame function) =
    reparameterize frame (derivativeIn (Frame2d.yDirection frame) function)

-- let fuuMagnitude = Range.maxAbs fuuBounds
-- let fuvMagnitude = Range.maxAbs fuvBounds
-- let fvvMagnitude = Range.maxAbs fvvBounds
-- let maxSecondDerivativeMagnitude = fuuMagnitude + 2 * fuvMagnitude + fvvMagnitude

-- isTangentPoint ::
--   Tolerance units =>
--   Solve1d.Neighborhood units ->
--   Derivatives units ->
--   Uv.Point ->
--   Bool
-- isTangentPoint neighborhood derivatives point = do
--   let Derivatives{f, fu, fv} = derivatives
--   let fIsZero = evaluateAt point f ~= Qty.zero
--   let derivativeTolerance = Solve1d.derivativeTolerance neighborhood 1
--   let fuIsZero = Qty.abs (evaluateAt point fu) <= derivativeTolerance
--   let fvIsZero = Qty.abs (evaluateAt point fv) <= derivativeTolerance
--   fIsZero && fuIsZero && fvIsZero

findCrossingSolution ::
  Tolerance units =>
  Derivatives units ->
  Subdomain ->
  DerivativeBounds units ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions Solution
findCrossingSolution derivatives subdomain derivativeBounds exclusions = do
  let DerivativeBounds{fBounds, fuBounds, fvBounds} = derivativeBounds
  if not (fBounds ^ Qty.zero)
    then Solve2d.pass
    else do
      case exclusions of
        Solve2d.SomeExclusions _ -> Solve2d.recurse
        Solve2d.NoExclusions -> do
          let fuSign = Solve1d.resolvedSign fuBounds
          let fvSign = Solve1d.resolvedSign fvBounds
          let fuResolved = fuSign /= Nothing
          let fvResolved = fvSign /= Nothing
          let uvBounds = Solve2d.bounds subdomain
          let uvInterior = Solve2d.interior subdomain
          let (uRange, vRange) = Bounds2d.coordinates uvBounds
          let (uInterior, vInterior) = Bounds2d.coordinates uvInterior
          let (uLeft, uRight) = Range.endpoints uRange
          let (vBottom, vTop) = Range.endpoints vRange
          let leftSolution = Maybe.map2 (,) fvSign (solveVertically derivatives uLeft vInterior)
          let rightSolution = Maybe.map2 (,) fvSign (solveVertically derivatives uRight vInterior)
          let bottomSolution = Maybe.map2 (,) fuSign (solveHorizontally derivatives uInterior vBottom)
          let topSolution = Maybe.map2 (,) fuSign (solveHorizontally derivatives uInterior vTop)
          let leftBoundary = Solve2d.leftBoundary subdomain
          let rightBoundary = Solve2d.rightBoundary subdomain
          let bottomBoundary = Solve2d.bottomBoundary subdomain
          let topBoundary = Solve2d.topBoundary subdomain
          let horizontalSolution startBoundary endBoundary uStart uEnd vBounds monotonic =
                CrossingCurveSolution $
                  PartialZeros.CrossingCurve startBoundary endBoundary $
                    NonEmpty.singleton (horizontalCurve derivatives uStart uEnd vBounds monotonic)
          let verticalSolution startBoundary endBoundary vStart vEnd uBounds monotonic =
                CrossingCurveSolution $
                  PartialZeros.CrossingCurve startBoundary endBoundary $
                    NonEmpty.singleton (verticalCurve derivatives uBounds vStart vEnd monotonic)
          case (leftSolution, rightSolution, bottomSolution, topSolution) of
            (Just (vSign, vLeft), Just (_, vRight), Nothing, Nothing) -> do
              -- Horizontal curve, fv is resolved
              let (uStart, uEnd, startBoundary, endBoundary) = case vSign of
                    Positive -> (uRight, uLeft, rightBoundary, leftBoundary)
                    Negative -> (uLeft, uRight, leftBoundary, rightBoundary)
              let solution = horizontalSolution startBoundary endBoundary uStart uEnd
              if fuResolved
                then Solve2d.return (solution (Range.from vLeft vRight) True)
                else do
                  let vBounds = parallelogramBounds uLeft uRight vLeft vRight (-fuBounds / fvBounds)
                  if Range.contains vBounds vInterior
                    then Solve2d.return (solution vBounds False)
                    else Solve2d.recurse
            (Nothing, Nothing, Just (uSign, uBottom), Just (_, uTop)) -> do
              -- Vertical curve, fu is resolved
              let (vStart, vEnd, startBoundary, endBoundary) = case uSign of
                    Positive -> (vBottom, vTop, bottomBoundary, topBoundary)
                    Negative -> (vTop, vBottom, topBoundary, bottomBoundary)
              let solution = verticalSolution startBoundary endBoundary vStart vEnd
              if fvResolved
                then Solve2d.return (solution (Range.from uBottom uTop) True)
                else do
                  let uBounds = parallelogramBounds vBottom vTop uBottom uTop (-fvBounds / fuBounds)
                  if Range.contains uBounds uInterior
                    then Solve2d.return (solution uBounds False)
                    else Solve2d.recurse
            (Just (vSign, vLeft), Nothing, Just (_, uBottom), Nothing) ->
              -- Bottom left diagonal, fu and fv are resolved
              Solve2d.return $
                if uBottom - uLeft >= vLeft - vBottom
                  then case vSign of
                    Positive -> horizontalSolution bottomBoundary leftBoundary uBottom uLeft (Range.from vBottom vLeft) True
                    Negative -> horizontalSolution leftBoundary bottomBoundary uLeft uBottom (Range.from vBottom vLeft) True
                  else case vSign of
                    Positive -> verticalSolution bottomBoundary leftBoundary vBottom vLeft (Range.from uLeft uBottom) True
                    Negative -> verticalSolution leftBoundary bottomBoundary vLeft vBottom (Range.from uLeft uBottom) True
            (Just (vSign, vLeft), Nothing, Nothing, Just (_, uTop)) ->
              -- Top left diagonal, fu and fv are resolved
              Solve2d.return $
                if uTop - uLeft >= vTop - vLeft
                  then case vSign of
                    Positive -> horizontalSolution topBoundary leftBoundary uTop uLeft (Range.from vLeft vTop) True
                    Negative -> horizontalSolution leftBoundary topBoundary uLeft uTop (Range.from vLeft vTop) True
                  else case vSign of
                    Positive -> verticalSolution topBoundary leftBoundary vTop vLeft (Range.from uLeft uTop) True
                    Negative -> verticalSolution leftBoundary topBoundary vLeft vTop (Range.from uLeft uTop) True
            (Nothing, Just (vSign, vRight), Just (_, uBottom), Nothing) ->
              -- Bottom right diagonal, fu and fv are resolved
              Solve2d.return $
                if uRight - uBottom >= vRight - vBottom
                  then case vSign of
                    Positive -> horizontalSolution rightBoundary bottomBoundary uRight uBottom (Range.from vBottom vRight) True
                    Negative -> horizontalSolution bottomBoundary rightBoundary uBottom uRight (Range.from vBottom vRight) True
                  else case vSign of
                    Positive -> verticalSolution rightBoundary bottomBoundary vRight vBottom (Range.from uBottom uRight) True
                    Negative -> verticalSolution bottomBoundary rightBoundary vBottom vRight (Range.from uBottom uRight) True
            (Nothing, Just (vSign, vRight), Nothing, Just (_, uTop)) ->
              -- Top right diagonal, fu and fv are resolved
              Solve2d.return $
                if uRight - uTop >= vTop - vRight
                  then case vSign of
                    Positive -> horizontalSolution rightBoundary topBoundary uRight uTop (Range.from vRight vTop) True
                    Negative -> horizontalSolution topBoundary rightBoundary uTop uRight (Range.from vRight vTop) True
                  else case vSign of
                    Positive -> verticalSolution rightBoundary topBoundary vRight vTop (Range.from uTop uRight) True
                    Negative -> verticalSolution topBoundary rightBoundary vTop vRight (Range.from uTop uRight) True
            _ -> Solve2d.recurse

solveVertically :: Tolerance units => Derivatives units -> Float -> Range Unitless -> Maybe Float
solveVertically derivatives u vRange = do
  let Derivatives{f, fv} = derivatives
  let fValue v = evaluate f (Point2d.xy u v)
  let fvValue v = evaluate fv (Point2d.xy u v)
  let v0 = Solve1d.monotonic fValue fvValue vRange
  if fValue v0 ~= Qty.zero then Just v0 else Nothing

solveHorizontally :: Tolerance units => Derivatives units -> Range Unitless -> Float -> Maybe Float
solveHorizontally derivatives uRange v = do
  let Derivatives{f, fu} = derivatives
  let fValue u = evaluate f (Point2d.xy u v)
  let fuValue u = evaluate fu (Point2d.xy u v)
  let u0 = Solve1d.monotonic fValue fuValue uRange
  if fValue u0 ~= Qty.zero then Just u0 else Nothing

horizontalCurve ::
  Tolerance units =>
  Derivatives units ->
  Float ->
  Float ->
  Range Unitless ->
  Bool ->
  Curve2d Uv.Coordinates
horizontalCurve derivatives uStart uEnd vRange monotonic = do
  let Derivatives{fu, fv} = derivatives
  Curve2d.wrap $
    HorizontalCurve
      { derivatives
      , dvdu = -fu / fv
      , uStart
      , uEnd
      , vRange
      , monotonic
      , tolerance = ?tolerance
      }

verticalCurve ::
  Tolerance units =>
  Derivatives units ->
  Range Unitless ->
  Float ->
  Float ->
  Bool ->
  Curve2d Uv.Coordinates
verticalCurve derivatives uRange vStart vEnd monotonic = do
  let Derivatives{fu, fv} = derivatives
  Curve2d.wrap $
    VerticalCurve
      { derivatives
      , dudv = -fv / fu
      , uRange
      , vStart
      , vEnd
      , monotonic
      , tolerance = ?tolerance
      }

data HorizontalCurve units = HorizontalCurve
  { derivatives :: Derivatives units
  , dvdu :: Function Unitless
  , uStart :: Float
  , uEnd :: Float
  , vRange :: Range Unitless
  , monotonic :: Bool
  , tolerance :: Qty units
  }
  deriving (Show)

solveForV :: HorizontalCurve units -> Float -> Float
solveForV (HorizontalCurve{derivatives, vRange, tolerance}) u = Tolerance.using tolerance do
  let Derivatives{f, fv} = derivatives
  let fValue v = evaluate f (Point2d.xy u v)
  let fvValue v = evaluate fv (Point2d.xy u v)
  Solve1d.monotonic fValue fvValue vRange

instance Curve2d.Interface (HorizontalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t curve = do
    let (HorizontalCurve{uStart, uEnd}) = curve
    let u = Float.interpolateFrom uStart uEnd t
    let v = solveForV curve u
    Point2d.xy u v

  segmentBoundsImpl t curve = do
    let (HorizontalCurve{dvdu, uStart, uEnd, vRange, monotonic}) = curve
    let (t1, t2) = Range.endpoints t
    let u1 = Float.interpolateFrom uStart uEnd t1
    let u2 = Float.interpolateFrom uStart uEnd t2
    let v1 = solveForV curve u1
    let v2 = solveForV curve u2
    if monotonic
      then Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      else do
        let slopeBounds = bounds dvdu (Bounds2d.xy (Range.from u1 u2) vRange)
        let vBounds = parallelogramBounds u1 u2 v1 v2 slopeBounds
        Bounds2d.xy (Range.from u1 u2) vBounds

  derivativeImpl crossingCurve@(HorizontalCurve{dvdu, uStart, uEnd}) = do
    let deltaU = uEnd - uStart
    let dudt = Curve1d.constant deltaU
    let dvdt = deltaU * Curve1d.wrap (CurveOnSurface crossingCurve dvdu)
    VectorCurve2d.xy dudt dvdt

  reverseImpl (HorizontalCurve{derivatives, dvdu, uStart, uEnd, vRange, monotonic, tolerance}) =
    HorizontalCurve{derivatives, dvdu, uStart = uEnd, uEnd = uStart, vRange, monotonic, tolerance}

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl Range.unit crossingCurve

  transformByImpl transform crossingCurve =
    Curve2d.wrap (Curve2d.TransformBy transform crossingCurve)

data VerticalCurve units = VerticalCurve
  { derivatives :: Derivatives units
  , dudv :: Function Unitless
  , uRange :: Range Unitless
  , vStart :: Float
  , vEnd :: Float
  , monotonic :: Bool
  , tolerance :: Qty units
  }
  deriving (Show)

solveForU :: VerticalCurve units -> Float -> Float
solveForU (VerticalCurve{derivatives, uRange, tolerance}) v = Tolerance.using tolerance do
  let Derivatives{f, fu} = derivatives
  let fValue u = evaluate f (Point2d.xy u v)
  let fuValue u = evaluate fu (Point2d.xy u v)
  Solve1d.monotonic fValue fuValue uRange

instance Curve2d.Interface (VerticalCurve units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t curve = do
    let (VerticalCurve{vStart, vEnd}) = curve
    let v = Float.interpolateFrom vStart vEnd t
    let u = solveForU curve v
    Point2d.xy u v

  segmentBoundsImpl t curve = do
    let (VerticalCurve{dudv, uRange, vStart, vEnd, monotonic}) = curve
    let (t1, t2) = Range.endpoints t
    let v1 = Float.interpolateFrom vStart vEnd t1
    let v2 = Float.interpolateFrom vStart vEnd t2
    let u1 = solveForU curve v1
    let u2 = solveForU curve v2
    if monotonic
      then Bounds2d.xy (Range.from u1 u2) (Range.from v1 v2)
      else do
        let slopeBounds = bounds dudv (Bounds2d.xy uRange (Range.from v1 v2))
        let uBounds = parallelogramBounds v1 v2 u1 u2 slopeBounds
        Bounds2d.xy uBounds (Range.from v1 v2)

  derivativeImpl crossingCurve@(VerticalCurve{dudv, vStart, vEnd}) = do
    let deltaV = vEnd - vStart
    let dvdt = Curve1d.constant deltaV
    let dudt = deltaV * Curve1d.wrap (CurveOnSurface crossingCurve dudv)
    VectorCurve2d.xy dudt dvdt

  reverseImpl (VerticalCurve{derivatives, dudv, uRange, vStart, vEnd, monotonic, tolerance}) =
    VerticalCurve{derivatives, dudv, uRange, vStart = vEnd, vEnd = vStart, monotonic, tolerance}

  boundsImpl crossingCurve = Curve2d.segmentBoundsImpl Range.unit crossingCurve

  transformByImpl transform crossingCurve =
    Curve2d.wrap (Curve2d.TransformBy transform crossingCurve)

parallelogramBounds ::
  Float ->
  Float ->
  Float ->
  Float ->
  Range Unitless ->
  Range Unitless
parallelogramBounds x1 x2 y1 y2 slopeBounds = do
  let (minSlope, maxSlope) = Range.endpoints slopeBounds
  let deltaX = x2 - x1
  let deltaY = y2 - y1
  let deltaXLow = (maxSlope * deltaX - deltaY) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
  let deltaXHigh = (deltaY - minSlope * deltaX) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
  let yLow = y1 + minSlope * deltaXLow
  let yHigh = y1 + maxSlope * deltaXHigh
  Range.hull4 y1 y2 yLow yHigh
