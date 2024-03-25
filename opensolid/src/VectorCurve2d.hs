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
  , squaredMagnitude
  , squaredMagnitude_
  , reverse
  , zeros
  , ZeroEverywhere (ZeroEverywhere)
  , HasZero (HasZero)
  , xComponent
  , yComponent
  , DerivativeHasZero (DerivativeHasZero)
  , direction
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  )
where

import Angle qualified
import Basis2d (Basis2d)
import Basis2d qualified
import Curve1d (Curve1d (Curve1d))
import Curve1d qualified
import Curve1d.Root qualified
import Direction2d (Direction2d)
import Direction2d qualified
import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} DirectionCurve2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Parameter qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Result qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds2d qualified

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Parameter.Bounds -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve2d (space @ units)
  Zero ::
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
  Product1d2d ::
    Curve1d units1 ->
    VectorCurve2d (space @ units2) ->
    VectorCurve2d (space @ (units1 :*: units2))
  Product2d1d ::
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ (units1 :*: units2))
  Quotient ::
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
  BezierCurve ::
    NonEmpty (Vector2d (space @ units)) ->
    VectorCurve2d (space @ units)

deriving instance Show (VectorCurve2d (space @ units))

instance HasUnits (VectorCurve2d (space @ units)) where
  type Units (VectorCurve2d (space @ units)) = units
  type Erase (VectorCurve2d (space @ units)) = VectorCurve2d (space @ Unitless)

instance
  space ~ space' =>
  Units.Coercion (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2))
  where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance Interface (VectorCurve2d (space @ units)) (space @ units) where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

instance Negation (VectorCurve2d (space @ units)) where
  negate Zero = Zero
  negate (Constant value) = Constant -value
  negate (XY x y) = XY -x -y
  negate (Negated c) = c
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product1d2d c1 c2) = Product1d2d -c1 c2
  negate (Product2d1d c1 c2) = Product2d1d c1 -c2
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
  space ~ space' =>
  Addition
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance
  space ~ space' =>
  Addition
    (VectorCurve2d (space @ units))
    (Vector2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  space ~ space' =>
  Addition
    (Vector2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  vector + curve = constant vector + curve

instance
  space ~ space' =>
  Subtraction
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance
  space ~ space' =>
  Subtraction
    (VectorCurve2d (space @ units))
    (Vector2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  space ~ space' =>
  Subtraction
    (Vector2d (space @ units))
    (VectorCurve2d (space' @ units))
    (VectorCurve2d (space @ units))
  where
  vector - curve = constant vector + curve

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) where
  type Curve1d units1 .*. VectorCurve2d (space @ units2) = VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product1d2d c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Product (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Qty units1) (VectorCurve2d (space @ units2)) where
  type Qty units1 .*. VectorCurve2d (space @ units2) = VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Curve1d.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (Curve1d units1) (Vector2d (space @ units2)) where
  type Curve1d units1 .*. Vector2d (space @ units2) = VectorCurve2d (space @ (units1 :*: units2))
  curve .*. vector = curve .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Product (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type VectorCurve2d (space @ units1) .*. Curve1d units2 = VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product2d1d c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Product (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Multiplication (VectorCurve2d (space @ units1)) (Qty units2) where
  type VectorCurve2d (space @ units1) .*. Qty units2 = VectorCurve2d (space @ (units1 :*: units2))
  curve .*. value = curve .*. Curve1d.constant value

instance
  Units.Product units1 units2 units3 =>
  Product (Vector2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication (Vector2d (space @ units1)) (Curve1d units2) where
  type Vector2d (space @ units1) .*. Curve1d units2 = VectorCurve2d (space @ (units1 :*: units2))
  vector .*. curve = constant vector .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Division (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type VectorCurve2d (space @ units1) ./. Curve1d units2 = VectorCurve2d (space @ (units1 :/: units2))
  c1 ./. c2 = Quotient c1 c2 -- TODO add special cases

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Division (VectorCurve2d (space @ units1)) (Qty units2) where
  type VectorCurve2d (space @ units1) ./. Qty units2 = VectorCurve2d (space @ (units1 :/: units2))
  curve ./. value = curve ./. Curve1d.constant value

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (DotProductOf space units1 units2)

instance Curve1d.Interface (DotProductOf space units1 units2) (units1 :*: units2) where
  evaluateAtImpl t (DotProductOf c1 c2) = evaluateAt t c1 .<>. evaluateAt t c2
  segmentBoundsImpl t (DotProductOf c1 c2) = segmentBounds t c1 .<>. segmentBounds t c2
  derivativeImpl (DotProductOf c1 c2) = derivative c1 .<>. c2 + c1 .<>. derivative c2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  DotMultiplication (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2))
  where
  type VectorCurve2d (space @ units1) .<>. VectorCurve2d (space' @ units2) = Curve1d (units1 :*: units2)
  curve1 .<>. curve2 = Curve1d (DotProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct (VectorCurve2d (space @ units1)) (Vector2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  DotMultiplication (VectorCurve2d (space @ units1)) (Vector2d (space' @ units2))
  where
  type VectorCurve2d (space @ units1) .<>. Vector2d (space' @ units2) = Curve1d (units1 :*: units2)
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct (Vector2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  DotMultiplication (Vector2d (space @ units1)) (VectorCurve2d (space' @ units2))
  where
  type Vector2d (space @ units1) .<>. VectorCurve2d (space' @ units2) = Curve1d (units1 :*: units2)
  vector .<>. curve = constant vector .<>. curve

instance
  space ~ space' =>
  DotProduct (VectorCurve2d (space @ units)) (Direction2d space') (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (VectorCurve2d (space @ units)) (Direction2d space')
  where
  type VectorCurve2d (space @ units) .<>. Direction2d space' = Curve1d (units :*: Unitless)
  curve .<>. direction2d = curve .<>. Direction2d.unitVector direction2d

instance
  space ~ space' =>
  DotProduct (Direction2d space) (VectorCurve2d (space' @ units)) (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (Direction2d space) (VectorCurve2d (space' @ units))
  where
  type Direction2d space .<>. VectorCurve2d (space' @ units) = Curve1d (Unitless :*: units)
  direction2d .<>. curve = Direction2d.unitVector direction2d .<>. curve

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))

deriving instance Show (CrossProductOf space units1 units2)

instance Curve1d.Interface (CrossProductOf space units1 units2) (units1 :*: units2) where
  evaluateAtImpl t (CrossProductOf c1 c2) = evaluateAt t c1 .><. evaluateAt t c2
  segmentBoundsImpl t (CrossProductOf c1 c2) = segmentBounds t c1 .><. segmentBounds t c2
  derivativeImpl (CrossProductOf c1 c2) = derivative c1 .><. c2 + c1 .><. derivative c2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  CrossMultiplication (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2))
  where
  type VectorCurve2d (space @ units1) .><. VectorCurve2d (space' @ units2) = Curve1d (units1 :*: units2)
  curve1 .><. curve2 = Curve1d (CrossProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct (VectorCurve2d (space @ units1)) (Vector2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  CrossMultiplication (VectorCurve2d (space @ units1)) (Vector2d (space' @ units2))
  where
  type VectorCurve2d (space @ units1) .><. Vector2d (space' @ units2) = Curve1d (units1 :*: units2)
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct (Vector2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3)

instance
  space ~ space' =>
  CrossMultiplication (Vector2d (space @ units1)) (VectorCurve2d (space' @ units2))
  where
  type Vector2d (space @ units1) .><. VectorCurve2d (space' @ units2) = Curve1d (units1 :*: units2)
  vector .><. curve = constant vector .><. curve

instance
  space ~ space' =>
  CrossProduct
    (VectorCurve2d (space @ units))
    (Direction2d space')
    (Curve1d units)

instance
  space ~ space' =>
  CrossMultiplication (VectorCurve2d (space @ units)) (Direction2d space')
  where
  type VectorCurve2d (space @ units) .><. Direction2d space' = Curve1d (units :*: Unitless)
  curve .><. direction2d = curve .><. Direction2d.unitVector direction2d

instance
  space ~ space' =>
  CrossProduct (Direction2d space) (VectorCurve2d (space' @ units)) (Curve1d units)

instance space ~ space' => CrossMultiplication (Direction2d space) (VectorCurve2d (space' @ units)) where
  type Direction2d space .><. VectorCurve2d (space' @ units) = Curve1d (Unitless :*: units)
  direction2d .><. curve = Direction2d.unitVector direction2d .><. curve

wrap :: Interface curve (space @ units) => curve -> VectorCurve2d (space @ units)
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

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
bezierCurve = BezierCurve

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
  NonEmpty.map (segmentControlVector a b controlVectors) <|
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
    Zero -> Vector2d.zero
    Constant value -> value
    Coerce c -> Units.coerce (evaluateAt t c)
    Reversed c -> evaluateAt (1.0 - t) c
    XY x y -> Vector2d.xy (Curve1d.evaluateAt t x) (Curve1d.evaluateAt t y)
    Negated c -> -(evaluateAt t c)
    Sum c1 c2 -> evaluateAt t c1 + evaluateAt t c2
    Difference c1 c2 -> evaluateAt t c1 - evaluateAt t c2
    Product1d2d c1 c2 -> Curve1d.evaluateAt t c1 .*. evaluateAt t c2
    Product2d1d c1 c2 -> evaluateAt t c1 .*. Curve1d.evaluateAt t c2
    Quotient c1 c2 -> evaluateAt t c1 ./. Curve1d.evaluateAt t c2
    PlaceInBasis basis c -> Vector2d.placeInBasis basis (evaluateAt t c)
    Line v1 v2 -> Vector2d.interpolateFrom v1 v2 t
    Arc r a b -> Vector2d.polar r (Qty.interpolateFrom a b t)
    QuadraticSpline v1 v2 v3 -> quadraticBlossom v1 v2 v3 t t
    CubicSpline v1 v2 v3 v4 -> cubicBlossom v1 v2 v3 v4 t t t
    BezierCurve controlVectors -> deCasteljau t controlVectors

segmentBounds :: Parameter.Bounds -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
segmentBounds t@(Range tl th) curve =
  case curve of
    VectorCurve2d c -> segmentBoundsImpl t c
    Zero -> VectorBounds2d.constant Vector2d.zero
    Constant value -> VectorBounds2d.constant value
    Coerce c -> Units.coerce (segmentBounds t c)
    Reversed c -> segmentBounds (1.0 - t) c
    XY x y -> VectorBounds2d (Curve1d.segmentBounds t x) (Curve1d.segmentBounds t y)
    Negated c -> -(segmentBounds t c)
    Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
    Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
    Product1d2d c1 c2 -> Curve1d.segmentBounds t c1 .*. segmentBounds t c2
    Product2d1d c1 c2 -> segmentBounds t c1 .*. Curve1d.segmentBounds t c2
    Quotient c1 c2 -> segmentBounds t c1 ./. Curve1d.segmentBounds t c2
    PlaceInBasis basis c -> VectorBounds2d.placeInBasis basis (segmentBounds t c)
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
    BezierCurve controlVectors ->
      VectorBounds2d.hullN (segmentControlVectors tl th controlVectors)

derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
derivative curve =
  case curve of
    VectorCurve2d c -> derivativeImpl c
    Zero -> Zero
    Constant _ -> Zero
    Coerce c -> Units.coerce (derivative c)
    Reversed c -> negate (reverse (derivative c))
    XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
    Negated c -> -(derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product1d2d c1 c2 -> Curve1d.derivative c1 .*. c2 + c1 .*. derivative c2
    Product2d1d c1 c2 -> derivative c1 .*. c2 + c1 .*. Curve1d.derivative c2
    Quotient c1 c2 -> (derivative c1 .*. c2 - c1 .*. Curve1d.derivative c2) .!/.! Curve1d.squared_ c2
    PlaceInBasis basis c -> PlaceInBasis basis (derivative c)
    Line v1 v2 -> constant (v2 - v1)
    Arc r a b -> do
      let sweptAngle = b - a
      let rotation = Angle.quarterTurn * Qty.sign sweptAngle
      Arc (r * Qty.abs (Angle.inRadians sweptAngle)) (a + rotation) (b + rotation)
    QuadraticSpline v1 v2 v3 -> line (2.0 * (v2 - v1)) (2.0 * (v3 - v2))
    CubicSpline v1 v2 v3 v4 -> quadraticSpline (3.0 * (v2 - v1)) (3.0 * (v3 - v2)) (3.0 * (v4 - v3))
    BezierCurve (_ :| []) -> Zero
    BezierCurve (v1 :| v2 : rest) -> do
      let degree = 1 + List.length rest
      BezierCurve (controlVectorDifferences (Float.fromInt degree) v1 v2 rest)

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve =
  case curve of
    VectorCurve2d _ -> Reversed curve
    Zero -> Zero
    Constant _ -> curve
    Coerce c -> Units.coerce (reverse c)
    Reversed c -> c
    XY x y -> XY (Curve1d.reverse x) (Curve1d.reverse y)
    Negated c -> Negated (reverse c)
    Sum c1 c2 -> Sum (reverse c1) (reverse c2)
    Difference c1 c2 -> Difference (reverse c1) (reverse c2)
    Product1d2d c1 c2 -> Product1d2d (Curve1d.reverse c1) (reverse c2)
    Product2d1d c1 c2 -> Product2d1d (reverse c1) (Curve1d.reverse c2)
    Quotient c1 c2 -> Quotient (reverse c1) (Curve1d.reverse c2)
    PlaceInBasis basis c -> PlaceInBasis basis (reverse c)
    Line v1 v2 -> Line v2 v1
    Arc r a b -> Arc r b a
    QuadraticSpline v1 v2 v3 -> QuadraticSpline v3 v2 v1
    CubicSpline v1 v2 v3 v4 -> CubicSpline v4 v3 v2 v1
    BezierCurve controlVectors -> BezierCurve (NonEmpty.reverse controlVectors)

newtype SquaredMagnitude (coordinateSystem :: CoordinateSystem) = SquaredMagnitude (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitude (space @ units))

instance Curve1d.Interface (SquaredMagnitude (space @ units)) (units :*: units) where
  evaluateAtImpl t (SquaredMagnitude curve) = Vector2d.squaredMagnitude_ (evaluateAt t curve)
  segmentBoundsImpl t (SquaredMagnitude curve) = VectorBounds2d.squaredMagnitude_ (segmentBounds t curve)
  derivativeImpl (SquaredMagnitude curve) = 2.0 * curve .<>. derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

squaredMagnitude_ :: VectorCurve2d (space @ units) -> Curve1d (units :*: units)
squaredMagnitude_ curve = Curve1d (SquaredMagnitude curve)

newtype Magnitude (coordinateSystem :: CoordinateSystem) = Magnitude (VectorCurve2d coordinateSystem)

deriving instance Show (Magnitude (space @ units))

instance Curve1d.Interface (Magnitude (space @ units)) units where
  evaluateAtImpl t (Magnitude curve) = Vector2d.magnitude (VectorCurve2d.evaluateAt t curve)
  segmentBoundsImpl t (Magnitude curve) = VectorBounds2d.magnitude (VectorCurve2d.segmentBounds t curve)
  derivativeImpl (Magnitude curve) = (VectorCurve2d.derivative curve .<>. curve) .!/! Curve1d (Magnitude curve)

data HasZero = HasZero deriving (Eq, Show, Error)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve1d units)
magnitude curve = case zeros curve of
  Error ZeroEverywhere -> Error HasZero
  Ok (NonEmpty _) -> Error HasZero
  Ok [] -> Ok (Curve1d (Magnitude curve))

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, Error)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Result ZeroEverywhere (List Float)
zeros curve = Result.do
  roots1d <- Curve1d.zeros (squaredMagnitude_ curve) ?? Error ZeroEverywhere
  Ok (List.map Curve1d.Root.value roots1d)
 where
  ?tolerance = Qty.squared_ ?tolerance

xComponent :: VectorCurve2d (space @ units) -> Curve1d units
xComponent curve = curve <> Direction2d.x

yComponent :: VectorCurve2d (space @ units) -> Curve1d units
yComponent curve = curve <> Direction2d.y

data DerivativeHasZero = DerivativeHasZero deriving (Eq, Show, Error)

direction :: Tolerance units => VectorCurve2d (space @ units) -> Result DerivativeHasZero (DirectionCurve2d space)
direction curve = case magnitude curve of
  Ok m -> Ok (DirectionCurve2d.unsafe (curve / m))
  Error HasZero -> Error DerivativeHasZero

placeIn :: Frame2d (global @ frameUnits) (Defines local) -> VectorCurve2d (local @ units) -> VectorCurve2d (global @ units)
placeIn globalFrame = placeInBasis (Frame2d.basis globalFrame)

relativeTo :: Frame2d (global @ frameUnits) (Defines local) -> VectorCurve2d (global @ units) -> VectorCurve2d (local @ units)
relativeTo globalFrame = relativeToBasis (Frame2d.basis globalFrame)

placeInBasis :: Basis2d global (Defines local) -> VectorCurve2d (local @ units) -> VectorCurve2d (global @ units)
placeInBasis globalBasis (PlaceInBasis basis curve) = PlaceInBasis (Basis2d.placeInBasis globalBasis basis) curve
placeInBasis globalBasis curve = PlaceInBasis globalBasis curve

relativeToBasis :: Basis2d global (Defines local) -> VectorCurve2d (global @ units) -> VectorCurve2d (local @ units)
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
