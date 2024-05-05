-- Needed for 'Curve1d * Vector2d = VectorCurve2d'
-- and 'Vector2d * Curve1d = VectorCurve2d' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Curve1d or Vector2d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorCurve2d
  ( VectorCurve2d (VectorCurve2d)
  , Interface (..)
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
  , bezierCurve
  , magnitude
  , unsafeMagnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , zeros
  , Zeros (ZeroEverywhere, Zeros)
  , HasZeros (HasZeros)
  , xComponent
  , yComponent
  , direction
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import Angle qualified
import Basis2d (Basis2d)
import Basis2d qualified
import CoordinateSystem (Space)
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Direction2d (Direction2d)
import Direction2d qualified
import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Tolerance qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds2d qualified
import VectorCurve2d.Direction qualified

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Range Unitless -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  transformByImpl ::
    Transform2d a (Space coordinateSystem @ transformUnits) ->
    curve ->
    VectorCurve2d coordinateSystem

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve2d (space @ units)
  Constant ::
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Coerce ::
    VectorCurve2d (space @ units1) ->
    VectorCurve2d (space @ units2)
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
  Product1d2d' ::
    Curve1d units1 ->
    VectorCurve2d (space @ units2) ->
    VectorCurve2d (space @ (units1 :*: units2))
  Product2d1d' ::
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ (units1 :*: units2))
  Quotient' ::
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ (units1 :/: units2))
  PlaceInBasis ::
    Basis2d global (Defines local) ->
    VectorCurve2d (local @ units) ->
    VectorCurve2d (global @ units)
  Line ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Arc ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
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
  BezierCurve ::
    NonEmpty (Vector2d (space @ units)) ->
    VectorCurve2d (space @ units)
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    VectorCurve2d (space @ units2) ->
    VectorCurve2d (space @ units2)

deriving instance Show (VectorCurve2d (space @ units))

instance HasUnits (VectorCurve2d (space @ units)) where
  type Units (VectorCurve2d (space @ units)) = units
  type Erase (VectorCurve2d (space @ units)) = VectorCurve2d (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (VectorCurve2d (space @ units1)) (VectorCurve2d (space_ @ units2))
  where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance Interface (VectorCurve2d (space @ units)) (space @ units) where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  transformByImpl = transformBy

instance Negation (VectorCurve2d (space @ units)) where
  negate (Constant value) = Constant -value
  negate (XY x y) = XY -x -y
  negate (Negated c) = c
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product1d2d' c1 c2) = Product1d2d' -c1 c2
  negate (Product2d1d' c1 c2) = Product2d1d' c1 -c2
  negate curve = Negated curve

instance Product Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

instance Multiplication Sign (VectorCurve2d (space @ units)) where
  type Sign .*. VectorCurve2d (space @ units) = VectorCurve2d (space @ (Unitless :*: units))
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Product (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units))

instance Multiplication (VectorCurve2d (space @ units)) Sign where
  type VectorCurve2d (space @ units) .*. Sign = VectorCurve2d (space @ (units :*: Unitless))
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  vector + curve = constant vector + curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  vector - curve = constant vector + curve

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) where
  type
    Curve1d units1 .*. VectorCurve2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product1d2d' c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Product (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Qty units1) (VectorCurve2d (space @ units2)) where
  type
    Qty units1 .*. VectorCurve2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Curve1d.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Curve1d units1) (Vector2d (space @ units2)) where
  type
    Curve1d units1 .*. Vector2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  curve .*. vector = curve .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Product (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve2d (space @ units1) .*. Curve1d units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product2d1d' c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Product (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Multiplication (VectorCurve2d (space @ units1)) (Qty units2) where
  type
    VectorCurve2d (space @ units1) .*. Qty units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  curve .*. value = curve .*. Curve1d.constant value

instance
  Units.Product units1 units2 units3 =>
  Product (Vector2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication (Vector2d (space @ units1)) (Curve1d units2) where
  type
    Vector2d (space @ units1) .*. Curve1d units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  vector .*. curve = constant vector .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Division (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve2d (space @ units1) ./. Curve1d units2 =
      VectorCurve2d (space @ (units1 :/: units2))
  c1 ./. c2 = Quotient' c1 c2 -- TODO add special cases

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Division (VectorCurve2d (space @ units1)) (Qty units2) where
  type
    VectorCurve2d (space @ units1) ./. Qty units2 =
      VectorCurve2d (space @ (units1 :/: units2))
  curve ./. value = curve ./. Curve1d.constant value

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (DotProductOf space units1 units2)

instance Curve1d.Interface (DotProductOf space units1 units2) (units1 :*: units2) where
  evaluateAtImpl t (DotProductOf c1 c2) = evaluateAt t c1 .<>. evaluateAt t c2
  segmentBoundsImpl t (DotProductOf c1 c2) = segmentBounds t c1 .<>. segmentBounds t c2
  derivativeImpl (DotProductOf c1 c2) = derivative c1 .<>. c2 + c1 .<>. derivative c2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  DotMultiplication (VectorCurve2d (space @ units1)) (VectorCurve2d (space_ @ units2))
  where
  type
    VectorCurve2d (space @ units1) .<>. VectorCurve2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  curve1 .<>. curve2 = Curve1d.wrap (DotProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotProduct (VectorCurve2d (space @ units1)) (Vector2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  DotMultiplication (VectorCurve2d (space @ units1)) (Vector2d (space_ @ units2))
  where
  type
    VectorCurve2d (space @ units1) .<>. Vector2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotProduct (Vector2d (space @ units1)) (VectorCurve2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  DotMultiplication (Vector2d (space @ units1)) (VectorCurve2d (space_ @ units2))
  where
  type
    Vector2d (space @ units1) .<>. VectorCurve2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  vector .<>. curve = constant vector .<>. curve

instance
  space ~ space_ =>
  DotProduct (VectorCurve2d (space @ units)) (Direction2d space_) (Curve1d units)

instance
  space ~ space_ =>
  DotMultiplication (VectorCurve2d (space @ units)) (Direction2d space_)
  where
  type VectorCurve2d (space @ units) .<>. Direction2d space_ = Curve1d (units :*: Unitless)
  curve .<>. direction2d = curve .<>. Direction2d.vector direction2d

instance
  space ~ space_ =>
  DotProduct (Direction2d space) (VectorCurve2d (space_ @ units)) (Curve1d units)

instance
  space ~ space_ =>
  DotMultiplication (Direction2d space) (VectorCurve2d (space_ @ units))
  where
  type Direction2d space .<>. VectorCurve2d (space_ @ units) = Curve1d (Unitless :*: units)
  direction2d .<>. curve = Direction2d.vector direction2d .<>. curve

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (CrossProductOf space units1 units2)

instance Curve1d.Interface (CrossProductOf space units1 units2) (units1 :*: units2) where
  evaluateAtImpl t (CrossProductOf c1 c2) = evaluateAt t c1 .><. evaluateAt t c2
  segmentBoundsImpl t (CrossProductOf c1 c2) = segmentBounds t c1 .><. segmentBounds t c2
  derivativeImpl (CrossProductOf c1 c2) = derivative c1 .><. c2 + c1 .><. derivative c2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  CrossMultiplication (VectorCurve2d (space @ units1)) (VectorCurve2d (space_ @ units2))
  where
  type
    VectorCurve2d (space @ units1) .><. VectorCurve2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  curve1 .><. curve2 = Curve1d.wrap (CrossProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossProduct (VectorCurve2d (space @ units1)) (Vector2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  CrossMultiplication (VectorCurve2d (space @ units1)) (Vector2d (space_ @ units2))
  where
  type
    VectorCurve2d (space @ units1) .><. Vector2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossProduct (Vector2d (space @ units1)) (VectorCurve2d (space_ @ units2)) (Curve1d units3)

instance
  space ~ space_ =>
  CrossMultiplication (Vector2d (space @ units1)) (VectorCurve2d (space_ @ units2))
  where
  type
    Vector2d (space @ units1) .><. VectorCurve2d (space_ @ units2) =
      Curve1d (units1 :*: units2)
  vector .><. curve = constant vector .><. curve

instance
  space ~ space_ =>
  CrossProduct
    (VectorCurve2d (space @ units))
    (Direction2d space_)
    (Curve1d units)

instance
  space ~ space_ =>
  CrossMultiplication (VectorCurve2d (space @ units)) (Direction2d space_)
  where
  type VectorCurve2d (space @ units) .><. Direction2d space_ = Curve1d (units :*: Unitless)
  curve .><. direction2d = curve .><. Direction2d.vector direction2d

instance
  space ~ space_ =>
  CrossProduct (Direction2d space) (VectorCurve2d (space_ @ units)) (Curve1d units)

instance
  space ~ space_ =>
  CrossMultiplication (Direction2d space) (VectorCurve2d (space_ @ units))
  where
  type Direction2d space .><. VectorCurve2d (space_ @ units) = Curve1d (Unitless :*: units)
  direction2d .><. curve = Direction2d.vector direction2d .><. curve

transformBy ::
  Transform2d a (space @ units1) ->
  VectorCurve2d (space @ units2) ->
  VectorCurve2d (space @ units2)
transformBy transform curve = do
  let t = Units.erase (Transform2d.toAffine transform)
  case curve of
    VectorCurve2d c -> VectorCurve2d (transformByImpl transform c)
    Constant v -> Constant (Vector2d.transformBy t v)
    Coerce c -> Coerce (VectorCurve2d.transformBy transform c)
    Reversed c -> Reversed (transformBy transform c)
    XY _ _ -> Transformed t curve
    Negated c -> Negated (transformBy transform c)
    Sum c1 c2 -> Sum (transformBy transform c1) (transformBy transform c2)
    Difference c1 c2 -> Difference (transformBy transform c1) (transformBy transform c2)
    Product1d2d' curve1d curve2d -> Product1d2d' curve1d (transformBy transform curve2d)
    Product2d1d' curve2d curve1d -> Product2d1d' (transformBy transform curve2d) curve1d
    Quotient' curve2d curve1d -> Quotient' (transformBy transform curve2d) curve1d
    PlaceInBasis basis c -> do
      let localTransform = Transform2d.relativeTo (Frame2d.at Point2d.origin basis) transform
      PlaceInBasis basis (transformBy localTransform c)
    Line v1 v2 -> Line (Vector2d.transformBy t v1) (Vector2d.transformBy t v2)
    Arc vx vy a b -> Arc (Vector2d.transformBy t vx) (Vector2d.transformBy t vy) a b
    QuadraticSpline v1 v2 v3 ->
      QuadraticSpline
        (Vector2d.transformBy t v1)
        (Vector2d.transformBy t v2)
        (Vector2d.transformBy t v3)
    CubicSpline v1 v2 v3 v4 ->
      CubicSpline
        (Vector2d.transformBy t v1)
        (Vector2d.transformBy t v2)
        (Vector2d.transformBy t v3)
        (Vector2d.transformBy t v4)
    BezierCurve controlVectors ->
      BezierCurve (NonEmpty.map (Vector2d.transformBy t) controlVectors)
    Transformed existing c -> Transformed (existing >> t) c

wrap :: Interface curve (space @ units) => curve -> VectorCurve2d (space @ units)
wrap = VectorCurve2d

zero :: VectorCurve2d (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
constant = Constant

xy :: Curve1d units -> Curve1d units -> VectorCurve2d (space @ units)
xy = XY

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
line v1 v2 = if v1 == v2 then Constant v1 else Line v1 v2

arc ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  VectorCurve2d (space @ units)
arc v1 v2 a b
  | v1 == Vector2d.zero && v2 == Vector2d.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = Arc v1 v2 a b

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

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
bezierCurve = BezierCurve

quadraticBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Vector2d (space @ units)
quadraticBlossom (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) t1 t2 = do
  let r1 = 1.0 - t1
  let r2 = 1.0 - t2
  let s1 = r1 * r2
  let s2 = r1 * t2 + t1 * r2
  let s3 = t1 * t2
  let x = s1 * x1 + s2 * x2 + s3 * x3
  let y = s1 * y1 + s2 * y2 + s3 * y3
  Vector2d x y

cubicBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Float ->
  Vector2d (space @ units)
cubicBlossom (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) t1 t2 t3 = do
  let r1 = 1.0 - t1
  let r2 = 1.0 - t2
  let r3 = 1.0 - t3
  let s1 = r1 * r2 * r3
  let s2 = r1 * r2 * t3 + r1 * t2 * r3 + t1 * r2 * r3
  let s3 = t1 * t2 * r3 + t1 * r2 * t3 + r1 * t2 * t3
  let s4 = t1 * t2 * t3
  let x = s1 * x1 + s2 * x2 + s3 * x3 + s4 * x4
  let y = s1 * y1 + s2 * y2 + s3 * y3 + s4 * y4
  Vector2d x y

deCasteljau :: Float -> NonEmpty (Vector2d (space @ units)) -> Vector2d (space @ units)
deCasteljau _ (vector :| []) = vector
deCasteljau t (v1 :| v2 : rest) = deCasteljau t (deCasteljauStep t v1 v2 rest)

deCasteljauStep ::
  Float ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
deCasteljauStep t v1 v2 rest = do
  let vector = Vector2d.interpolateFrom v1 v2 t
  case rest of
    [] -> NonEmpty.singleton vector
    v3 : remaining -> NonEmpty.prepend vector (deCasteljauStep t v2 v3 remaining)

segmentControlVectors ::
  Float ->
  Float ->
  NonEmpty (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
segmentControlVectors a b controlVectors =
  NonEmpty.map (segmentControlVector a b controlVectors) $
    NonEmpty.range 0 (NonEmpty.length controlVectors - 1)

segmentControlVector ::
  Float ->
  Float ->
  NonEmpty (Vector2d (space @ units)) ->
  Int ->
  Vector2d (space @ units)
segmentControlVector _ _ (vector :| []) _ = vector
segmentControlVector a b (p1 :| p2 : ps) n = do
  let t = if n > 0 then b else a
  let reduced = deCasteljauStep t p1 p2 ps
  segmentControlVector a b reduced (n - 1)

controlVectorDifferences ::
  Float ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
controlVectorDifferences scale v1 v2 rest = do
  let difference = scale * (v2 - v1)
  case rest of
    [] -> NonEmpty.singleton difference
    v3 : remaining -> NonEmpty.prepend difference (controlVectorDifferences scale v2 v3 remaining)

evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
evaluateAt t curve =
  case curve of
    VectorCurve2d c -> evaluateAtImpl t c
    Constant value -> value
    Coerce c -> Units.coerce (evaluateAt t c)
    Reversed c -> evaluateAt (1.0 - t) c
    XY x y -> Vector2d.xy (Curve1d.evaluateAt t x) (Curve1d.evaluateAt t y)
    Negated c -> -(evaluateAt t c)
    Sum c1 c2 -> evaluateAt t c1 + evaluateAt t c2
    Difference c1 c2 -> evaluateAt t c1 - evaluateAt t c2
    Product1d2d' c1 c2 -> Curve1d.evaluateAt t c1 .*. evaluateAt t c2
    Product2d1d' c1 c2 -> evaluateAt t c1 .*. Curve1d.evaluateAt t c2
    Quotient' c1 c2 -> evaluateAt t c1 ./. Curve1d.evaluateAt t c2
    PlaceInBasis basis c -> Vector2d.placeInBasis basis (evaluateAt t c)
    Line v1 v2 -> Vector2d.interpolateFrom v1 v2 t
    Arc v1 v2 a b -> do
      let theta = Qty.interpolateFrom a b t
      Angle.cos theta * v1 + Angle.sin theta * v2
    QuadraticSpline v1 v2 v3 -> quadraticBlossom v1 v2 v3 t t
    CubicSpline v1 v2 v3 v4 -> cubicBlossom v1 v2 v3 v4 t t t
    BezierCurve controlVectors -> deCasteljau t controlVectors
    Transformed transform c -> Vector2d.transformBy transform (evaluateAt t c)

segmentBounds :: Range Unitless -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
segmentBounds t@(Range tl th) curve =
  case curve of
    VectorCurve2d c -> segmentBoundsImpl t c
    Constant value -> VectorBounds2d.constant value
    Coerce c -> Units.coerce (segmentBounds t c)
    Reversed c -> segmentBounds (1.0 - t) c
    XY x y -> VectorBounds2d (Curve1d.segmentBounds t x) (Curve1d.segmentBounds t y)
    Negated c -> -(segmentBounds t c)
    Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
    Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
    Product1d2d' c1 c2 -> Curve1d.segmentBounds t c1 .*. segmentBounds t c2
    Product2d1d' c1 c2 -> segmentBounds t c1 .*. Curve1d.segmentBounds t c2
    Quotient' c1 c2 -> segmentBounds t c1 ./. Curve1d.segmentBounds t c2
    PlaceInBasis basis c -> VectorBounds2d.placeInBasis basis (segmentBounds t c)
    Line v1 v2 ->
      VectorBounds2d.hull2
        (Vector2d.interpolateFrom v1 v2 tl)
        (Vector2d.interpolateFrom v1 v2 th)
    Arc v1 v2 a b -> do
      let theta = a + (b - a) * t
      Range.cos theta * v1 + Range.sin theta * v2
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
    BezierCurve controlVectors ->
      VectorBounds2d.hullN (segmentControlVectors tl th controlVectors)
    Transformed transform c -> VectorBounds2d.transformBy transform (segmentBounds t c)

derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve =
  case curve of
    VectorCurve2d c -> derivativeImpl c
    Constant _ -> zero
    Coerce c -> Units.coerce (derivative c)
    Reversed c -> negate (reverse (derivative c))
    XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
    Negated c -> -(derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product1d2d' c1 c2 -> Curve1d.derivative c1 .*. c2 + c1 .*. derivative c2
    Product2d1d' c1 c2 -> derivative c1 .*. c2 + c1 .*. Curve1d.derivative c2
    Quotient' c1 c2 ->
      (derivative c1 .*. c2 - c1 .*. Curve1d.derivative c2) .!/.! Curve1d.squared' c2
    PlaceInBasis basis c -> PlaceInBasis basis (derivative c)
    Line v1 v2 -> constant (v2 - v1)
    Arc v1 v2 a b -> do
      let dTheta = Angle.unitless (b - a)
      Arc (v2 * dTheta) (-v1 * dTheta) a b
    QuadraticSpline v1 v2 v3 -> line (2.0 * (v2 - v1)) (2.0 * (v3 - v2))
    CubicSpline v1 v2 v3 v4 ->
      quadraticSpline (3.0 * (v2 - v1)) (3.0 * (v3 - v2)) (3.0 * (v4 - v3))
    BezierCurve (_ :| []) -> zero
    BezierCurve (v1 :| v2 : rest) -> do
      let degree = 1 + List.length rest
      BezierCurve (controlVectorDifferences (Float.fromInt degree) v1 v2 rest)
    Transformed transform c -> transformBy transform (derivative c)

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve =
  case curve of
    VectorCurve2d _ -> Reversed curve
    Constant _ -> curve
    Coerce c -> Units.coerce (reverse c)
    Reversed c -> c
    XY x y -> XY (Curve1d.reverse x) (Curve1d.reverse y)
    Negated c -> Negated (reverse c)
    Sum c1 c2 -> Sum (reverse c1) (reverse c2)
    Difference c1 c2 -> Difference (reverse c1) (reverse c2)
    Product1d2d' c1 c2 -> Product1d2d' (Curve1d.reverse c1) (reverse c2)
    Product2d1d' c1 c2 -> Product2d1d' (reverse c1) (Curve1d.reverse c2)
    Quotient' c1 c2 -> Quotient' (reverse c1) (Curve1d.reverse c2)
    PlaceInBasis basis c -> PlaceInBasis basis (reverse c)
    Line v1 v2 -> Line v2 v1
    Arc v1 v2 a b -> Arc v1 v2 b a
    QuadraticSpline v1 v2 v3 -> QuadraticSpline v3 v2 v1
    CubicSpline v1 v2 v3 v4 -> CubicSpline v4 v3 v2 v1
    BezierCurve controlVectors -> BezierCurve (NonEmpty.reverse controlVectors)
    Transformed transform c -> Transformed transform (reverse c)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  evaluateAtImpl t (SquaredMagnitude' curve) =
    Vector2d.squaredMagnitude' (evaluateAt t curve)
  segmentBoundsImpl t (SquaredMagnitude' curve) =
    VectorBounds2d.squaredMagnitude' (segmentBounds t curve)
  derivativeImpl (SquaredMagnitude' curve) =
    2.0 * curve .<>. derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve2d (space @ units) -> Curve1d (units :*: units)
squaredMagnitude' curve = Curve1d.wrap (SquaredMagnitude' curve)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (VectorCurve2d coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Interface (NonZeroMagnitude (space @ units)) units where
  evaluateAtImpl t (NonZeroMagnitude curve) =
    Vector2d.magnitude (VectorCurve2d.evaluateAt t curve)
  segmentBoundsImpl t (NonZeroMagnitude curve) =
    VectorBounds2d.magnitude (VectorCurve2d.segmentBounds t curve)
  derivativeImpl (NonZeroMagnitude curve) =
    (VectorCurve2d.derivative curve .<>. curve) .!/! Curve1d.wrap (NonZeroMagnitude curve)

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve1d units
unsafeMagnitude curve = Curve1d.wrap (NonZeroMagnitude curve)

newtype HasZeros = HasZeros Zeros deriving (Eq, Show)

deriving anyclass instance Error HasZeros

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZeros (Curve1d units)
magnitude curve = case zeros curve of
  Zeros [] -> Ok (Curve1d.wrap (NonZeroMagnitude curve))
  Zeros someZeros -> Error (HasZeros (Zeros someZeros))
  ZeroEverywhere -> Error (HasZeros ZeroEverywhere)

data Zeros = ZeroEverywhere | Zeros (List Float) deriving (Eq, Show)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Zeros
zeros curve =
  Tolerance.using Tolerance.squared' $
    case Curve1d.zeros (squaredMagnitude' curve) of
      Curve1d.ZeroEverywhere -> ZeroEverywhere
      Curve1d.Zeros roots -> Zeros (List.map Curve1d.Root.value roots)

xComponent :: VectorCurve2d (space @ units) -> Curve1d units
xComponent curve = curve <> Direction2d.x

yComponent :: VectorCurve2d (space @ units) -> Curve1d units
yComponent curve = curve <> Direction2d.y

direction ::
  Tolerance units =>
  VectorCurve2d (space @ units) ->
  Result HasZeros (DirectionCurve2d space)
direction curve = case zeros curve of
  -- Definitely can't get the direction of a vector curve
  -- if that vector curve is zero everywhere!
  ZeroEverywhere -> Error (HasZeros ZeroEverywhere)
  -- If the vector curve is not zero anywhere,
  -- then we can safely compute its direction
  Zeros [] -> Ok (VectorCurve2d.Direction.unsafe curve (derivative curve))
  -- Otherwise, check where the vector curve is zero:
  -- if it's only zero at one or both endpoints,
  -- and the curve's *derivative* is non-zero at those endpoints,
  -- then it's still possible to uniquely determine a tangent direction everywhere
  Zeros parameterValues -> do
    let curveDerivative = derivative curve
    case List.filter (not . isRemovableDegeneracy curveDerivative) parameterValues of
      -- All degeneracies were removable, so we're good
      [] -> Ok (VectorCurve2d.Direction.unsafe curve curveDerivative)
      -- There were some non-removable degeneracies (interior cusps) left,
      -- so report them as an error
      interiorCusps -> Error (HasZeros (Zeros interiorCusps))

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative t =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (t == 0.0 || t == 1.0) && evaluateAt t curveDerivative != Vector2d.zero

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeIn globalFrame = placeInBasis (Frame2d.basis globalFrame)

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeTo globalFrame = relativeToBasis (Frame2d.basis globalFrame)

placeInBasis ::
  Basis2d global (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeInBasis globalBasis (PlaceInBasis basis curve) =
  PlaceInBasis (Basis2d.placeInBasis globalBasis basis) curve
placeInBasis globalBasis curve =
  PlaceInBasis globalBasis curve

relativeToBasis ::
  Basis2d global (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
