module VectorCurve2d
  ( VectorCurve2d (VectorCurve2d)
  , IsVectorCurve2d (..)
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
  , magnitude
  , normalize
  , reverse
  )
where

import Angle (Angle)
import Angle qualified
import Curve1d (Curve1d (Curve1d), IsCurve1d)
import Curve1d qualified
import Domain (Domain)
import Generic qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units (Unitless)
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBox2d (VectorBox2d (VectorBox2d))
import VectorBox2d qualified

class Show curve => IsVectorCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> VectorBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d :: forall curve coordinateSystem. IsVectorCurve2d curve coordinateSystem => curve -> VectorCurve2d coordinateSystem
  Zero :: VectorCurve2d (space @ units)
  Constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
  XY :: Curve1d units -> Curve1d units -> VectorCurve2d (space @ units)
  Negated :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
  Sum :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
  Difference :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
  Product1d2d :: forall space units1 units2 units3. Units.Product units1 units2 units3 => Curve1d units1 -> VectorCurve2d (space @ units2) -> VectorCurve2d (space @ units3)
  Product2d1d :: forall space units1 units2 units3. Units.Product units1 units2 units3 => VectorCurve2d (space @ units1) -> Curve1d units2 -> VectorCurve2d (space @ units3)
  Quotient :: forall space units1 units2 units3. Units.Quotient units1 units2 units3 => VectorCurve2d (space @ units1) -> Curve1d units2 -> VectorCurve2d (space @ units3)
  Line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
  Arc :: Qty units -> Angle -> Angle -> VectorCurve2d (space @ units)
  QuadraticSpline :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
  CubicSpline :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)

deriving instance Show (VectorCurve2d coordinateSystem)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (VectorCurve2d (space @ units1'))
      (VectorCurve2d (space' @ units2'))

instance IsVectorCurve2d (VectorCurve2d (space @ units)) (space @ units) where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse

instance Generic.Zero (VectorCurve2d (space @ units)) where
  zero = Zero

instance Negation (VectorCurve2d (space @ units)) where
  negate Zero = Zero
  negate (Constant value) = Constant -value
  negate (XY x y) = XY -x -y
  negate (Negated c) = c
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product1d2d c1 c2) = Product1d2d -c1 c2
  negate (Product2d1d c1 c2) = Product2d1d c1 -c2
  negate curve = Negated curve

instance space ~ space' => Addition (VectorCurve2d (space @ units)) (VectorCurve2d (space' @ units)) (VectorCurve2d (space @ units)) where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance space ~ space' => Addition (VectorCurve2d (space @ units)) (Vector2d (space' @ units)) (VectorCurve2d (space @ units)) where
  curve + vector = curve + constant vector

instance space ~ space' => Addition (Vector2d (space @ units)) (VectorCurve2d (space' @ units)) (VectorCurve2d (space @ units)) where
  vector + curve = constant vector + curve

instance space ~ space' => Subtraction (VectorCurve2d (space @ units)) (VectorCurve2d (space' @ units)) (VectorCurve2d (space @ units)) where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance space ~ space' => Subtraction (VectorCurve2d (space @ units)) (Vector2d (space' @ units)) (VectorCurve2d (space @ units)) where
  curve - vector = curve - constant vector

instance space ~ space' => Subtraction (Vector2d (space @ units)) (VectorCurve2d (space' @ units)) (VectorCurve2d (space @ units)) where
  vector - curve = constant vector + curve

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3)) where
  -- TODO add special cases
  c1 * c2 = Product1d2d c1 c2

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3)) where
  c1 * c2 = Curve1d.constant c1 * c2

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3)) where
  -- TODO add special cases
  c1 * c2 = Product2d1d c1 c2

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3)) where
  curve * value = curve * Curve1d.constant value

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3)) where
  -- TODO add special cases
  c1 / c2 = Quotient c1 c2

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3)) where
  curve / value = curve / Curve1d.constant value

data DotProductOf space units1 units2 = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (DotProductOf space units1 units2)

instance Units.Product units1 units2 units3 => IsCurve1d (DotProductOf space units1 units2) units3 where
  evaluateAtImpl t (DotProductOf c1 c2) = evaluateAt t c1 <> evaluateAt t c2
  segmentBoundsImpl t (DotProductOf c1 c2) = segmentBounds t c1 <> segmentBounds t c2
  derivativeImpl (DotProductOf c1 c2) = derivative c1 <> c2 + c1 <> derivative c2

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3) where
  -- TODO add special cases
  curve1 <> curve2 = Curve1d (DotProductOf curve1 curve2)

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (VectorCurve2d (space @ units1)) (Vector2d (space' @ units2)) (Curve1d units3) where
  curve <> vector = curve <> constant vector

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (Vector2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3) where
  vector <> curve = constant vector <> curve

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

quadraticSpline
  :: Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> VectorCurve2d (space @ units)
quadraticSpline = QuadraticSpline

cubicSpline
  :: Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> VectorCurve2d (space @ units)
cubicSpline = CubicSpline

quadraticBlossom
  :: Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Float
  -> Float
  -> Vector2d (space @ units)
quadraticBlossom (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) t1 t2 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      s1 = r1 * r2
      s2 = r1 * t2 + t1 * r2
      s3 = t1 * t2
      x = s1 * x1 + s2 * x2 + s3 * x3
      y = s1 * y1 + s2 * y2 + s3 * y3
   in Vector2d x y

cubicBlossom
  :: Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Float
  -> Float
  -> Float
  -> Vector2d (space @ units)
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

segmentBounds :: Domain -> VectorCurve2d (space @ units) -> VectorBox2d (space @ units)
segmentBounds t@(Range tl th) curve =
  case curve of
    VectorCurve2d c -> segmentBoundsImpl t c
    Zero -> VectorBox2d.constant Vector2d.zero
    Constant value -> VectorBox2d.constant value
    XY x y -> VectorBox2d (Curve1d.segmentBounds t x) (Curve1d.segmentBounds t y)
    Negated c -> -(segmentBounds t c)
    Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
    Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
    Product1d2d c1 c2 -> Curve1d.segmentBounds t c1 * segmentBounds t c2
    Product2d1d c1 c2 -> segmentBounds t c1 * Curve1d.segmentBounds t c2
    Quotient c1 c2 -> segmentBounds t c1 / Curve1d.segmentBounds t c2
    Line v1 v2 ->
      VectorBox2d.hull2
        (Vector2d.interpolateFrom v1 v2 tl)
        (Vector2d.interpolateFrom v1 v2 th)
    Arc r a b ->
      VectorBox2d.polar (Range.constant r) (a + (b - a) * t)
    QuadraticSpline v1 v2 v3 ->
      VectorBox2d.hull3
        (quadraticBlossom v1 v2 v3 tl tl)
        (quadraticBlossom v1 v2 v3 tl th)
        (quadraticBlossom v1 v2 v3 th th)
    CubicSpline v1 v2 v3 v4 ->
      VectorBox2d.hull4
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
    XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
    Negated c -> -(derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product1d2d c1 c2 -> Curve1d.derivative c1 * c2 + c1 * derivative c2
    Product2d1d c1 c2 -> derivative c1 * c2 + c1 * Curve1d.derivative c2
    Quotient c1 c2 -> derivative c1 / c2 + curve * (Curve1d.derivative c2 / c2)
    Line v1 v2 -> constant (v2 - v1)
    Arc r a b -> Arc (r * Angle.inRadians (b - a)) (a + Angle.quarterTurn) (b + Angle.quarterTurn)
    QuadraticSpline v1 v2 v3 -> line (2.0 * (v2 - v1)) (2.0 * (v3 - v2))
    CubicSpline v1 v2 v3 v4 -> quadraticSpline (3.0 * (v2 - v1)) (3.0 * (v3 - v2)) (3.0 * (v4 - v3))

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve =
  case curve of
    VectorCurve2d c -> VectorCurve2d (reverseImpl c)
    Zero -> Zero
    Constant _ -> curve
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

newtype MagnitudeOf (coordinateSystem :: CoordinateSystem) = MagnitudeOf (VectorCurve2d coordinateSystem)

deriving instance Show (MagnitudeOf coordinateSystem)

instance IsCurve1d (MagnitudeOf (space @ units)) units where
  evaluateAtImpl t (MagnitudeOf curve) = Vector2d.magnitude (evaluateAt t curve)
  segmentBoundsImpl t (MagnitudeOf curve) = VectorBox2d.magnitude (segmentBounds t curve)
  derivativeImpl (MagnitudeOf curve) = derivative curve <> normalize curve

newtype SquaredMagnitudeOf (coordinateSystem :: CoordinateSystem) = SquaredMagnitudeOf (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitudeOf coordinateSystem)

instance Units.Squared units1 units2 => IsCurve1d (SquaredMagnitudeOf (space @ units1)) units2 where
  evaluateAtImpl t (SquaredMagnitudeOf curve) = Vector2d.squaredMagnitude (evaluateAt t curve)
  segmentBoundsImpl t (SquaredMagnitudeOf curve) = VectorBox2d.squaredMagnitude (segmentBounds t curve)
  derivativeImpl (SquaredMagnitudeOf curve) = 2.0 * curve <> derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Curve1d (SquaredMagnitudeOf curve)

magnitude :: VectorCurve2d (space @ units) -> Curve1d units
magnitude curve = Curve1d (MagnitudeOf curve)

normalize :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ Unitless)
normalize curve = curve / magnitude curve
