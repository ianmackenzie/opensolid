module VectorCurve2d
  ( VectorCurve2d (VectorCurve2d)
  , IsVectorCurve2d (..)
  , wrap
  , evaluateAt
  , segmentBounds
  , derivative
  , zero
  , constant
  , xy
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , squaredMagnitude
  , reverse
  , roots
  , ZeroEverywhere (ZeroEverywhere)
  , xComponent
  , yComponent
  , DegenerateCurve (DegenerateCurve)
  , direction
  )
where

import Angle qualified
import Curve1d (Curve1d (Curve1d), IsCurve1d)
import Curve1d qualified
import Curve1d.Root qualified as Root
import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import Domain (Domain)
import Generic qualified
import List qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds2d qualified
import VectorCurve2d.Direction qualified

class
  (Show curve) =>
  IsVectorCurve2d curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    (IsVectorCurve2d curve (space @ units)) =>
    curve ->
    VectorCurve2d (space @ units)
  Zero ::
    VectorCurve2d (space @ units)
  Constant ::
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Reversed ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  XY ::
    Curve1d units ->
    Curve1d units ->
    VectorCurve2d (space @ units)
  Negated ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Sum ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Difference ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Product1d2d ::
    (Units.Product units1 units2 units3) =>
    Curve1d units1 ->
    VectorCurve2d (space @ units2) ->
    VectorCurve2d (space @ units3)
  Product2d1d ::
    (Units.Product units1 units2 units3) =>
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ units3)
  Quotient ::
    (Units.Quotient units1 units2 units3) =>
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ units3)
  Line ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Arc ::
    Qty units ->
    Angle ->
    Angle ->
    VectorCurve2d (space @ units)
  QuadraticSpline ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  CubicSpline ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)

deriving instance Show (VectorCurve2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (VectorCurve2d (space @ units1'))
    (VectorCurve2d (space' @ units2'))

instance IsVectorCurve2d (VectorCurve2d (space @ units)) (space @ units) where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

instance Generic.HasZero (VectorCurve2d (space @ units)) where
  zeroImpl = zero

instance Negation (VectorCurve2d (space @ units)) where
  negate Zero = Zero
  negate (Constant value) = Constant -value
  negate (XY x y) = XY -x -y
  negate (Negated c) = c
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product1d2d c1 c2) = Product1d2d -c1 c2
  negate (Product2d1d c1 c2) = Product2d1d c1 -c2
  negate curve = Negated curve

instance
  Multiplication
    Sign
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  Positive * curve = curve
  Negative * curve = -curve

instance
  Multiplication
    (VectorCurve2d (space @ units))
    Sign
    (VectorCurve2d (space @ units))
  where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space ~ space') =>
  Addition
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance
  (space ~ space') =>
  Addition
    (VectorCurve2d (space @ units))
    (Vector2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  (space ~ space') =>
  Addition
    (Vector2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  vector + curve = constant vector + curve

instance
  (space ~ space') =>
  Subtraction
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance
  (space ~ space') =>
  Subtraction
    (VectorCurve2d (space @ units))
    (Vector2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  (space ~ space') =>
  Subtraction
    (Vector2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  vector - curve = constant vector + curve

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Curve1d units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))
  where
  -- TODO add special cases
  c1 * c2 = Product1d2d c1 c2

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))
  where
  c1 * c2 = Curve1d.constant c1 * c2

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ units3))
  where
  -- TODO add special cases
  c1 * c2 = Product2d1d c1 c2

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ units3))
  where
  curve * value = curve * Curve1d.constant value

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ units3))
  where
  -- TODO add special cases
  c1 / c2 = Quotient c1 c2

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ units3))
  where
  curve / value = curve / Curve1d.constant value

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (DotProductOf space units1 units2)

instance
  (Units.Product units1 units2 units3) =>
  IsCurve1d (DotProductOf space units1 units2) units3
  where
  evaluateAtImpl t (DotProductOf c1 c2) = evaluateAt t c1 <> evaluateAt t c2
  segmentBoundsImpl t (DotProductOf c1 c2) = segmentBounds t c1 <> segmentBounds t c2
  derivativeImpl (DotProductOf c1 c2) = derivative c1 <> c2 + c1 <> derivative c2

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  DotProduct
    (VectorCurve2d (space @ units1))
    (VectorCurve2d (space' @ units2))
    (Curve1d units3)
  where
  -- TODO add special cases
  curve1 <> curve2 = Curve1d (DotProductOf curve1 curve2)

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  DotProduct
    (VectorCurve2d (space @ units1))
    (Vector2d (space' @ units2))
    (Curve1d units3)
  where
  curve <> vector = curve <> constant vector

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  DotProduct
    (Vector2d (space @ units1))
    (VectorCurve2d (space' @ units2))
    (Curve1d units3)
  where
  vector <> curve = constant vector <> curve

instance
  (space ~ space') =>
  DotProduct
    (VectorCurve2d (space @ units))
    (Direction2d space')
    (Curve1d units)
  where
  curve <> Direction2d vector = curve <> vector

instance
  (space ~ space') =>
  DotProduct
    (Direction2d space)
    (VectorCurve2d (space' @ units))
    (Curve1d units)
  where
  Direction2d vector <> curve = vector <> curve

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (CrossProductOf space units1 units2)

instance
  (Units.Product units1 units2 units3) =>
  IsCurve1d (CrossProductOf space units1 units2) units3
  where
  evaluateAtImpl t (CrossProductOf c1 c2) = evaluateAt t c1 >< evaluateAt t c2
  segmentBoundsImpl t (CrossProductOf c1 c2) = segmentBounds t c1 >< segmentBounds t c2
  derivativeImpl (CrossProductOf c1 c2) = derivative c1 >< c2 + c1 >< derivative c2

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  CrossProduct
    (VectorCurve2d (space @ units1))
    (VectorCurve2d (space' @ units2))
    (Curve1d units3)
  where
  -- TODO add special cases
  curve1 >< curve2 = Curve1d (CrossProductOf curve1 curve2)

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  CrossProduct
    (VectorCurve2d (space @ units1))
    (Vector2d (space' @ units2))
    (Curve1d units3)
  where
  curve >< vector = curve >< constant vector

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  CrossProduct
    (Vector2d (space @ units1))
    (VectorCurve2d (space' @ units2))
    (Curve1d units3)
  where
  vector >< curve = constant vector >< curve

instance
  (space ~ space') =>
  CrossProduct
    (VectorCurve2d (space @ units))
    (Direction2d space')
    (Curve1d units)
  where
  curve >< Direction2d vector = curve >< vector

instance
  (space ~ space') =>
  CrossProduct
    (Direction2d space)
    (VectorCurve2d (space' @ units))
    (Curve1d units)
  where
  Direction2d vector >< curve = vector >< curve

wrap :: (IsVectorCurve2d curve (space @ units)) => curve -> VectorCurve2d (space @ units)
wrap = VectorCurve2d

zero :: VectorCurve2d (space @ units)
zero = Zero

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
constant vector = if vector == Vector2d.zero then Zero else Constant vector

xy :: Curve1d units -> Curve1d units -> VectorCurve2d (space @ units)
xy = XY

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
line v1 v2 = if v1 == v2 then Constant v1 else Line v1 v2

arc :: Qty units -> Angle -> Angle -> VectorCurve2d (space @ units)
arc r a b
  | r == Qty.zero = Constant Vector2d.zero
  | a == b = Constant (Vector2d.polar r a)
  | otherwise = Arc r a b

quadraticSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
quadraticSpline = QuadraticSpline

cubicSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
cubicSpline = CubicSpline

quadraticBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Vector2d (space @ units)
quadraticBlossom (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) t1 t2 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      s1 = r1 * r2
      s2 = r1 * t2 + t1 * r2
      s3 = t1 * t2
      x = s1 * x1 + s2 * x2 + s3 * x3
      y = s1 * y1 + s2 * y2 + s3 * y3
   in Vector2d x y

cubicBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Float ->
  Vector2d (space @ units)
cubicBlossom (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) t1 t2 t3 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      r3 = 1.0 - t3
      s1 = r1 * r2 * r3
      s2 = r1 * r2 * t3 + r1 * t2 * r3 + t1 * r2 * r3
      s3 = t1 * t2 * r3 + t1 * r2 * t3 + r1 * t2 * t3
      s4 = t1 * t2 * t3
      x = s1 * x1 + s2 * x2 + s3 * x3 + s4 * x4
      y = s1 * y1 + s2 * y2 + s3 * y3 + s4 * y4
   in Vector2d x y

evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
evaluateAt t curve =
  case curve of
    VectorCurve2d c -> evaluateAtImpl t c
    Zero -> Vector2d.zero
    Constant value -> value
    Reversed c -> evaluateAt (1.0 - t) c
    XY x y -> Vector2d.xy (Curve1d.evaluateAt t x) (Curve1d.evaluateAt t y)
    Negated c -> -(evaluateAt t c)
    Sum c1 c2 -> evaluateAt t c1 + evaluateAt t c2
    Difference c1 c2 -> evaluateAt t c1 - evaluateAt t c2
    Product1d2d c1 c2 -> Curve1d.evaluateAt t c1 * evaluateAt t c2
    Product2d1d c1 c2 -> evaluateAt t c1 * Curve1d.evaluateAt t c2
    Quotient c1 c2 -> evaluateAt t c1 / Curve1d.evaluateAt t c2
    Line v1 v2 -> Vector2d.interpolateFrom v1 v2 t
    Arc r a b -> Vector2d.polar r (Qty.interpolateFrom a b t)
    QuadraticSpline v1 v2 v3 -> quadraticBlossom v1 v2 v3 t t
    CubicSpline v1 v2 v3 v4 -> cubicBlossom v1 v2 v3 v4 t t t

segmentBounds :: Domain -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
segmentBounds t@(Range tl th) curve =
  case curve of
    VectorCurve2d c -> segmentBoundsImpl t c
    Zero -> VectorBounds2d.constant Vector2d.zero
    Constant value -> VectorBounds2d.constant value
    Reversed c -> segmentBounds (1.0 - t) c
    XY x y -> VectorBounds2d (Curve1d.segmentBounds t x) (Curve1d.segmentBounds t y)
    Negated c -> -(segmentBounds t c)
    Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
    Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
    Product1d2d c1 c2 -> Curve1d.segmentBounds t c1 * segmentBounds t c2
    Product2d1d c1 c2 -> segmentBounds t c1 * Curve1d.segmentBounds t c2
    Quotient c1 c2 -> segmentBounds t c1 / Curve1d.segmentBounds t c2
    Line v1 v2 ->
      VectorBounds2d.hull2
        (Vector2d.interpolateFrom v1 v2 tl)
        (Vector2d.interpolateFrom v1 v2 th)
    Arc r a b ->
      VectorBounds2d.polar (Range.constant r) (a + (b - a) * t)
    QuadraticSpline v1 v2 v3 ->
      VectorBounds2d.hull3
        (quadraticBlossom v1 v2 v3 tl tl)
        (quadraticBlossom v1 v2 v3 tl th)
        (quadraticBlossom v1 v2 v3 th th)
    CubicSpline v1 v2 v3 v4 ->
      VectorBounds2d.hull4
        (cubicBlossom v1 v2 v3 v4 tl tl tl)
        (cubicBlossom v1 v2 v3 v4 tl tl th)
        (cubicBlossom v1 v2 v3 v4 tl th th)
        (cubicBlossom v1 v2 v3 v4 th th th)

derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve =
  case curve of
    VectorCurve2d c -> derivativeImpl c
    Zero -> Zero
    Constant _ -> Zero
    Reversed c -> negate (reverse (derivative c))
    XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
    Negated c -> -(derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product1d2d c1 c2 -> Curve1d.derivative c1 * c2 + c1 * derivative c2
    Product2d1d c1 c2 -> derivative c1 * c2 + c1 * Curve1d.derivative c2
    Quotient c1 c2 ->
      let c1' = Units.generalize c1
          c2' = Units.generalize c2
       in Units.specialize $
            (derivative c1' .* c2' - c1' .* Curve1d.derivative c2') ./ Curve1d.squared c2'
    Line v1 v2 -> constant (v2 - v1)
    Arc r a b ->
      let sweptAngle = b - a
          rotation = Angle.quarterTurn * Qty.sign sweptAngle
       in Arc (r * Qty.abs (Angle.inRadians sweptAngle)) (a + rotation) (b + rotation)
    QuadraticSpline v1 v2 v3 -> line (2.0 * (v2 - v1)) (2.0 * (v3 - v2))
    CubicSpline v1 v2 v3 v4 -> quadraticSpline (3.0 * (v2 - v1)) (3.0 * (v3 - v2)) (3.0 * (v4 - v3))

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve =
  case curve of
    VectorCurve2d _ -> Reversed curve
    Zero -> Zero
    Constant _ -> curve
    Reversed c -> c
    XY x y -> XY (Curve1d.reverse x) (Curve1d.reverse y)
    Negated c -> Negated (reverse c)
    Sum c1 c2 -> Sum (reverse c1) (reverse c2)
    Difference c1 c2 -> Difference (reverse c1) (reverse c2)
    Product1d2d c1 c2 -> Product1d2d (Curve1d.reverse c1) (reverse c2)
    Product2d1d c1 c2 -> Product2d1d (reverse c1) (Curve1d.reverse c2)
    Quotient c1 c2 -> Quotient (reverse c1) (Curve1d.reverse c2)
    Line v1 v2 -> Line v2 v1
    Arc r a b -> Arc r b a
    QuadraticSpline v1 v2 v3 -> QuadraticSpline v3 v2 v1
    CubicSpline v1 v2 v3 v4 -> CubicSpline v4 v3 v2 v1

newtype SquaredMagnitudeOf (coordinateSystem :: CoordinateSystem) = SquaredMagnitudeOf (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitudeOf (space @ units))

instance (Units.Squared units1 units2) => IsCurve1d (SquaredMagnitudeOf (space @ units1)) units2 where
  evaluateAtImpl t (SquaredMagnitudeOf curve) = Vector2d.squaredMagnitude (evaluateAt t curve)
  segmentBoundsImpl t (SquaredMagnitudeOf curve) = VectorBounds2d.squaredMagnitude (segmentBounds t curve)
  derivativeImpl (SquaredMagnitudeOf curve) = 2.0 * curve <> derivative curve

squaredMagnitude :: (Units.Squared units1 units2) => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Curve1d (SquaredMagnitudeOf curve)

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, ErrorMessage)

roots :: (Tolerance units) => VectorCurve2d (space @ units) -> Result ZeroEverywhere (List Float)
roots curve =
  case Curve1d.roots (squaredMagnitude (Units.generalize curve)) of
    Ok roots1d -> Ok (List.map Root.value roots1d)
    Error Curve1d.ZeroEverywhere -> Error VectorCurve2d.ZeroEverywhere
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

xComponent :: VectorCurve2d (space @ units) -> Curve1d units
xComponent curve = curve <> Direction2d.x

yComponent :: VectorCurve2d (space @ units) -> Curve1d units
yComponent curve = curve <> Direction2d.y

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, ErrorMessage)

direction :: (Tolerance units) => VectorCurve2d (space @ units) -> Result DegenerateCurve (DirectionCurve2d space)
direction curve =
  let curveDerivative = derivative curve
   in if isNondegenerate curve curveDerivative
        then Ok (VectorCurve2d.Direction.unsafe curve curveDerivative)
        else Error DegenerateCurve

isNondegenerate :: (Tolerance units) => VectorCurve2d (space @ units) -> VectorCurve2d (space @ units) -> Bool
isNondegenerate curve curveDerivative =
  case roots curve of
    Error VectorCurve2d.ZeroEverywhere -> False
    Ok degeneracies -> List.all (isRemovableDegeneracy curveDerivative) degeneracies

isRemovableDegeneracy :: (Tolerance units) => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative t =
  (t == 0.0 || t == 1.0) && VectorCurve2d.evaluateAt t curveDerivative != Vector2d.zero
