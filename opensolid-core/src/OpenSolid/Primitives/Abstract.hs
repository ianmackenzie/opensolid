module OpenSolid.Primitives.Abstract
  ( Vector
  , Direction
  , Point
  , VectorBounds
  , DirectionBounds
  , Bounds
  , VectorTransform
  , Transform
  , VectorExists (..)
  , DirectionExists (..)
  , PointExists (..)
  , VectorBoundsExists (..)
  , DirectionBoundsExists (..)
  , BoundsExists (..)
  , VectorTransformExists (..)
  , TransformExists (..)
  )
where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import GHC.TypeLits (Natural)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Error (IsZero (IsZero))
import OpenSolid.HasZero (HasZero)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Set qualified as Set
import OpenSolid.Sign qualified as Sign
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Units (HasUnits)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorTransform2D (VectorTransform2D)
import OpenSolid.VectorTransform2D qualified as VectorTransform2D
import OpenSolid.VectorTransform3D (VectorTransform3D)
import OpenSolid.VectorTransform3D qualified as VectorTransform3D

type family
  Vector dimension units space =
    vector | vector -> dimension units space
  where
  Vector 1 units Void = Quantity units
  Vector 2 units Void = Vector2D units
  Vector 3 units space = Vector3D units space

type family
  Direction (dimension :: Natural) space =
    direction | direction -> dimension space
  where
  Direction 1 Void = Sign
  Direction 2 Void = Direction2D
  Direction 3 space = Direction3D space

type family
  Point dimension units space =
    point | point -> dimension units space
  where
  Point 2 units Void = Point2D units
  Point 3 Meters space = Point3D space

type family
  VectorBounds dimension units space =
    vectorBounds | vectorBounds -> dimension units space
  where
  VectorBounds 1 units Void = Interval units
  VectorBounds 2 units Void = VectorBounds2D units
  VectorBounds 3 units space = VectorBounds3D units space

type family
  DirectionBounds dimension space =
    directionBounds | directionBounds -> dimension space
  where
  DirectionBounds 1 Void = Interval Unitless
  DirectionBounds 2 Void = DirectionBounds2D
  DirectionBounds 3 space = DirectionBounds3D space

type family
  Bounds dimension units space =
    bounds | bounds -> dimension units space
  where
  Bounds 2 units Void = Bounds2D units
  Bounds 3 Meters space = Bounds3D space

type family
  VectorTransform dimension tag space =
    transform | transform -> dimension tag space
  where
  VectorTransform 1 tag Void = VectorTransform1D tag
  VectorTransform 2 tag Void = VectorTransform2D tag
  VectorTransform 3 tag space = VectorTransform3D tag space

newtype VectorTransform1D tag = VectorTransform1D Number

type family
  Transform dimension tag units space =
    transform | transform -> dimension tag units space
  where
  Transform 2 tag units Void = Transform2D tag units
  Transform 3 tag Meters space = Transform3D tag space

class
  ( HasZero (Vector dimension units space)
  , HasUnits (Vector dimension units space) units
  , Coercible (Vector dimension units space) (Vector dimension Unitless space)
  , Coercible (Vector dimension Unitless space) (Vector dimension units space)
  , Eq (Vector dimension units space)
  , Ord (Vector dimension units space)
  , Show (Vector dimension units space)
  , ApproximateEquality (Vector dimension units space) (Tolerance units)
  , Negation (Vector dimension units space)
  , Addition
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Subtraction
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Multiplication Number (Vector dimension units space) (Vector dimension units space)
  , Multiplication (Vector dimension units space) Number (Vector dimension units space)
  , Multiplication (Quantity units) (Vector dimension Unitless space) (Vector dimension units space)
  , Multiplication (Vector dimension Unitless space) (Quantity units) (Vector dimension units space)
  , Division (Vector dimension units space) Number (Vector dimension units space)
  , Division (Vector dimension units space) (Quantity units) (Vector dimension Unitless space)
  , DotMultiplication
      (Vector dimension units space)
      (Vector dimension Unitless space)
      (Quantity units)
  , DotMultiplication
      (Vector dimension Unitless space)
      (Vector dimension units space)
      (Quantity units)
  , DotMultiplication
      (Vector dimension units space)
      (Direction dimension space)
      (Quantity units)
  , DotMultiplication
      (Direction dimension space)
      (Vector dimension units space)
      (Quantity units)
  , DotMultiplication_
      (Vector dimension units space)
      (Vector dimension units space)
      (Quantity (units ?*? units))
  , VectorExists dimension Unitless space
  , DirectionExists dimension space
  ) =>
  VectorExists dimension units space
  where
  vectorZero :: Vector dimension units space
  vectorSquaredMagnitude_ :: Vector dimension units space -> Quantity (units ?*? units)
  vectorMagnitude :: Vector dimension units space -> Quantity units
  vectorDirection ::
    Tolerance units =>
    Vector dimension units space ->
    Result IsZero (Direction dimension space)
  vectorMagnitudeAndDirection ::
    (VectorExists dimension units space, DirectionExists dimension space, Tolerance units) =>
    Vector dimension units space ->
    Result IsZero (Quantity units, Direction dimension space)
  vectorCrossProductMagnitude_ ::
    Vector dimension units space ->
    Vector dimension units space ->
    Quantity (units ?*? units)
  vectorComponentIn ::
    (VectorExists dimension units space, DirectionExists dimension space) =>
    Direction dimension space ->
    Vector dimension units space ->
    Quantity units
  vectorProjectionIn ::
    (VectorExists dimension units space, DirectionExists dimension space) =>
    Direction dimension space ->
    Vector dimension units space ->
    Vector dimension units space
  vectorTransformBy ::
    VectorTransform dimension tag space ->
    Vector dimension units space ->
    Vector dimension units space
  vectorSum :: List (Vector dimension units space) -> Vector dimension units space

instance VectorExists 1 units Void where
  {-# INLINE vectorZero #-}
  vectorZero = Quantity.zero
  {-# INLINE vectorSquaredMagnitude_ #-}
  vectorSquaredMagnitude_ = Quantity.squared_
  vectorMagnitude = Quantity.abs
  vectorDirection value =
    if value ~= Quantity.zero
      then Err IsZero
      else Ok (Quantity.sign value)
  vectorMagnitudeAndDirection value =
    if value ~= Quantity.zero
      then Err IsZero
      else Ok (Quantity.abs value, Quantity.sign value)
  vectorCrossProductMagnitude_ _ _ = Quantity.zero
  vectorComponentIn = (*)
  vectorProjectionIn _ value = value
  vectorTransformBy (VectorTransform1D scale) value = scale * value
  vectorSum = Quantity.sum

instance VectorExists 2 units Void where
  {-# INLINE vectorZero #-}
  vectorZero = Vector2D.zero
  {-# INLINE vectorSquaredMagnitude_ #-}
  vectorSquaredMagnitude_ vector = vector `dot_` vector
  vectorMagnitude = Vector2D.magnitude
  vectorDirection = Vector2D.direction
  vectorMagnitudeAndDirection = Vector2D.magnitudeAndDirection
  vectorCrossProductMagnitude_ v1 v2 = Quantity.abs (v1 `cross_` v2)
  vectorComponentIn = Vector2D.componentIn
  vectorProjectionIn = Vector2D.projectionIn
  vectorTransformBy transform vector = transform * vector
  vectorSum = Vector2D.sum

instance VectorExists 3 units space where
  {-# INLINE vectorZero #-}
  vectorZero = Vector3D.zero
  {-# INLINE vectorSquaredMagnitude_ #-}
  vectorSquaredMagnitude_ vector = vector `dot_` vector
  vectorMagnitude = Vector3D.magnitude
  vectorDirection = Vector3D.direction
  vectorMagnitudeAndDirection = Vector3D.magnitudeAndDirection
  vectorCrossProductMagnitude_ v1 v2 = Vector3D.magnitude (v1 `cross_` v2)
  vectorComponentIn = Vector3D.componentIn
  vectorProjectionIn = Vector3D.projectionIn
  vectorTransformBy transform vector = transform * vector
  vectorSum = Vector3D.sum

class
  ( VectorExists dimension Unitless space
  , Eq (Direction dimension space)
  , Ord (Direction dimension space)
  , Show (Direction dimension space)
  , Negation (Direction dimension space)
  , ApproximateEquality (Direction dimension space) ()
  , DotMultiplication (Direction dimension space) (Direction dimension space) Number
  ) =>
  DirectionExists (dimension :: Natural) (space :: Type)
  where
  directionUnsafe :: Vector dimension Unitless space -> Direction dimension space
  directionUnwrap :: Direction dimension space -> Vector dimension Unitless space
  directionAreParallel :: Direction dimension space -> Direction dimension space -> Bool
  directionAreIndependent :: Direction dimension space -> Direction dimension space -> Bool
  directionArePerpendicular :: Direction dimension space -> Direction dimension space -> Bool

instance DirectionExists 1 Void where
  {-# INLINE directionUnsafe #-}
  directionUnsafe = Quantity.sign
  {-# INLINE directionUnwrap #-}
  directionUnwrap = Sign.value
  {-# INLINE directionAreParallel #-}
  directionAreParallel _ _ = True
  {-# INLINE directionAreIndependent #-}
  directionAreIndependent _ _ = False
  {-# INLINE directionArePerpendicular #-}
  directionArePerpendicular _ _ = False

instance DirectionExists 2 Void where
  {-# INLINE directionUnsafe #-}
  directionUnsafe = Direction2D.unsafe
  {-# INLINE directionUnwrap #-}
  directionUnwrap = Direction2D.unwrap
  {-# INLINE directionAreParallel #-}
  directionAreParallel = Direction2D.areParallel
  {-# INLINE directionAreIndependent #-}
  directionAreIndependent = Direction2D.areIndependent
  {-# INLINE directionArePerpendicular #-}
  directionArePerpendicular = Direction2D.arePerpendicular

instance DirectionExists 3 space where
  {-# INLINE directionUnsafe #-}
  directionUnsafe = Direction3D.unsafe
  {-# INLINE directionUnwrap #-}
  directionUnwrap = Direction3D.unwrap
  {-# INLINE directionAreParallel #-}
  directionAreParallel = Direction3D.areParallel
  {-# INLINE directionAreIndependent #-}
  directionAreIndependent = Direction3D.areIndependent
  {-# INLINE directionArePerpendicular #-}
  directionArePerpendicular = Direction3D.arePerpendicular

class
  ( VectorExists dimension units space
  , BoundsExists dimension units space
  , Eq (Point dimension units space)
  , Ord (Point dimension units space)
  , Show (Point dimension units space)
  , ApproximateEquality (Point dimension units space) (Tolerance units)
  , Addition
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Point dimension units space)
      (Vector dimension units space)
  ) =>
  PointExists dimension units space
  where
  pointDistanceFrom :: Point dimension units space -> Point dimension units space -> Quantity units
  pointTransformBy ::
    Transform dimension tag units space ->
    Point dimension units space ->
    Point dimension units space

instance PointExists 2 units Void where
  {-# INLINE pointDistanceFrom #-}
  pointDistanceFrom = Point2D.distanceFrom
  pointTransformBy = Point2D.transformBy

instance PointExists 3 Meters space where
  {-# INLINE pointDistanceFrom #-}
  pointDistanceFrom = Point3D.distanceFrom
  pointTransformBy = Point3D.transformBy

class
  ( VectorExists dimension units space
  , VectorBoundsExists dimension Unitless space
  , DirectionBoundsExists dimension space
  , HasUnits (VectorBounds dimension units space) units
  , Coercible (VectorBounds dimension units space) (VectorBounds dimension Unitless space)
  , Coercible (VectorBounds dimension Unitless space) (VectorBounds dimension units space)
  , Show (VectorBounds dimension units space)
  , Negation (VectorBounds dimension units space)
  , Addition
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
  , Subtraction
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
  , Multiplication Number (VectorBounds dimension units space) (VectorBounds dimension units space)
  , Multiplication (VectorBounds dimension units space) Number (VectorBounds dimension units space)
  , Multiplication
      (Quantity units)
      (VectorBounds dimension Unitless space)
      (VectorBounds dimension units space)
  , Multiplication
      (VectorBounds dimension Unitless space)
      (Quantity units)
      (VectorBounds dimension units space)
  , Division
      (VectorBounds dimension units space)
      Number
      (VectorBounds dimension units space)
  , Division
      (VectorBounds dimension units space)
      (Quantity units)
      (VectorBounds dimension Unitless space)
  , Multiplication
      (Interval Unitless)
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
  , Multiplication
      (VectorBounds dimension units space)
      (Interval Unitless)
      (VectorBounds dimension units space)
  , Multiplication
      (Interval units)
      (VectorBounds dimension Unitless space)
      (VectorBounds dimension units space)
  , Multiplication
      (VectorBounds dimension Unitless space)
      (Interval units)
      (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) (Interval Unitless) (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) (Interval units) (VectorBounds dimension Unitless space)
  , DotMultiplication
      (VectorBounds dimension units space)
      (Vector dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (Vector dimension Unitless space)
      (VectorBounds dimension units space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension Unitless space)
      (Vector dimension units space)
      (Interval units)
  , DotMultiplication
      (Vector dimension units space)
      (VectorBounds dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension units space)
      (VectorBounds dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension Unitless space)
      (VectorBounds dimension units space)
      (Interval units)
  , DotMultiplication_
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (Interval (units ?*? units))
  ) =>
  VectorBoundsExists dimension units space
  where
  vectorBoundsMember :: Vector dimension units space -> VectorBounds dimension units space -> Bool
  vectorBoundsCenter :: VectorBounds dimension units space -> Vector dimension units space
  vectorBoundsSquaredMagnitude_ :: VectorBounds dimension units space -> Interval (units ?*? units)
  vectorBoundsMagnitude :: VectorBounds dimension units space -> Interval units
  vectorBoundsDirection :: VectorBounds dimension units space -> DirectionBounds dimension space
  vectorBoundsNormalize ::
    VectorBounds dimension units space ->
    VectorBounds dimension Unitless space
  vectorBoundsDiameter :: VectorBounds dimension units space -> Quantity units
  vectorBoundsIsResolved :: VectorBounds dimension units space -> Bool
  vectorBoundsAreDistinct ::
    VectorBounds dimension units space ->
    VectorBounds dimension units space ->
    Bool
  vectorBoundsAreIndependent ::
    VectorBounds dimension units space ->
    VectorBounds dimension units space ->
    Bool
  vectorBoundsTransformBy ::
    VectorTransform dimension tag space ->
    VectorBounds dimension units space ->
    VectorBounds dimension units space

instance VectorBoundsExists 1 units Void where
  vectorBoundsMember = Interval.member
  vectorBoundsCenter = Interval.midpoint
  vectorBoundsSquaredMagnitude_ = Interval.squared_
  vectorBoundsMagnitude = Interval.abs
  vectorBoundsDirection (Interval low high)
    | low > Quantity.zero = Interval.constant 1.0
    | high < Quantity.zero = Interval.constant -1.0
    | otherwise = Interval -1.0 1.0
  vectorBoundsNormalize (Interval low high)
    | low > Quantity.zero = Interval.constant 1.0
    | high < Quantity.zero = Interval.constant -1.0
    | otherwise = Interval -1.0 1.0
  vectorBoundsDiameter = Interval.width
  vectorBoundsIsResolved = Interval.isResolved
  vectorBoundsAreDistinct = Interval.areDistinct
  vectorBoundsAreIndependent _ _ = False
  vectorBoundsTransformBy (VectorTransform1D scale) value = scale * value

instance VectorBoundsExists 2 units Void where
  vectorBoundsMember = VectorBounds2D.member
  vectorBoundsCenter = VectorBounds2D.center
  vectorBoundsSquaredMagnitude_ = VectorBounds2D.squaredMagnitude_
  vectorBoundsMagnitude = VectorBounds2D.magnitude
  vectorBoundsDirection = VectorBounds2D.direction
  vectorBoundsNormalize = VectorBounds2D.normalize
  vectorBoundsDiameter = VectorBounds2D.diameter
  vectorBoundsIsResolved = VectorBounds2D.isResolved
  vectorBoundsAreDistinct = VectorBounds2D.areDistinct
  vectorBoundsAreIndependent = VectorBounds2D.areIndependent
  vectorBoundsTransformBy = VectorBounds2D.transformBy

instance VectorBoundsExists 3 units space where
  vectorBoundsMember = VectorBounds3D.member
  vectorBoundsCenter = VectorBounds3D.center
  vectorBoundsSquaredMagnitude_ = VectorBounds3D.squaredMagnitude_
  vectorBoundsMagnitude = VectorBounds3D.magnitude
  vectorBoundsDirection = VectorBounds3D.direction
  vectorBoundsNormalize = VectorBounds3D.normalize
  vectorBoundsDiameter = VectorBounds3D.diameter
  vectorBoundsIsResolved = VectorBounds3D.isResolved
  vectorBoundsAreDistinct = VectorBounds3D.areDistinct
  vectorBoundsAreIndependent = VectorBounds3D.areIndependent
  vectorBoundsTransformBy = VectorBounds3D.transformBy

class
  ( VectorBoundsExists dimension Unitless space
  , Show (DirectionBounds dimension space)
  , Negation (DirectionBounds dimension space)
  , DotMultiplication
      (DirectionBounds dimension space)
      (DirectionBounds dimension space)
      (Interval Unitless)
  ) =>
  DirectionBoundsExists dimension (space :: Type)
  where
  directionBoundsUnsafe :: VectorBounds dimension Unitless space -> DirectionBounds dimension space
  directionBoundsUnwrap :: DirectionBounds dimension space -> VectorBounds dimension Unitless space
  directionBoundsAreDistinct ::
    DirectionBounds dimension space ->
    DirectionBounds dimension space ->
    Bool
  directionBoundsAreIndependent ::
    DirectionBounds dimension space ->
    DirectionBounds dimension space ->
    Bool

instance DirectionBoundsExists 1 Void where
  {-# INLINE directionBoundsUnsafe #-}
  directionBoundsUnsafe = id
  {-# INLINE directionBoundsUnwrap #-}
  directionBoundsUnwrap = id
  {-# INLINE directionBoundsAreDistinct #-}
  directionBoundsAreDistinct = Interval.areDistinct
  {-# INLINE directionBoundsAreIndependent #-}
  directionBoundsAreIndependent _ _ = False

instance DirectionBoundsExists 2 Void where
  {-# INLINE directionBoundsUnsafe #-}
  directionBoundsUnsafe = DirectionBounds2D.unsafe
  {-# INLINE directionBoundsUnwrap #-}
  directionBoundsUnwrap = DirectionBounds2D.unwrap
  {-# INLINE directionBoundsAreDistinct #-}
  directionBoundsAreDistinct = DirectionBounds2D.areDistinct
  {-# INLINE directionBoundsAreIndependent #-}
  directionBoundsAreIndependent = DirectionBounds2D.areIndependent

instance DirectionBoundsExists 3 space where
  {-# INLINE directionBoundsUnsafe #-}
  directionBoundsUnsafe = DirectionBounds3D.unsafe
  {-# INLINE directionBoundsUnwrap #-}
  directionBoundsUnwrap = DirectionBounds3D.unwrap
  {-# INLINE directionBoundsAreDistinct #-}
  directionBoundsAreDistinct = DirectionBounds3D.areDistinct
  {-# INLINE directionBoundsAreIndependent #-}
  directionBoundsAreIndependent = DirectionBounds3D.areIndependent

class
  ( PointExists dimension units space
  , VectorBoundsExists dimension units space
  , Show (Bounds dimension units space)
  , Set.Bounds (Bounds dimension units space)
  , Intersects (Point dimension units space) (Bounds dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Point dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Bounds dimension units space) (Tolerance units)
  , Addition
      (Bounds dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Addition
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Subtraction
      (Bounds dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Subtraction
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Subtraction
      (Bounds dimension units space)
      (Bounds dimension units space)
      (VectorBounds dimension units space)
  ) =>
  BoundsExists dimension units space
  where
  boundsConstant :: Point dimension units space -> Bounds dimension units space
  boundsContains :: Bounds dimension units space -> Bounds dimension units space -> Bool
  boundsHull :: NonEmpty (Point dimension units space) -> Bounds dimension units space
  boundsHull2 ::
    Point dimension units space ->
    Point dimension units space ->
    Bounds dimension units space
  boundsAggregate :: NonEmpty (Bounds dimension units space) -> Bounds dimension units space
  boundsAggregate2 ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Bounds dimension units space
  boundsIntersection ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Maybe (Bounds dimension units space)
  boundsDiameter :: Bounds dimension units space -> Quantity units
  boundsTransformBy ::
    Transform dimension tag units space ->
    Bounds dimension units space ->
    Bounds dimension units space

instance BoundsExists 2 units Void where
  boundsConstant = Bounds2D.constant
  boundsContains = Bounds2D.contains
  boundsHull = Bounds2D.hull
  boundsHull2 = Bounds2D.hull2
  boundsAggregate = Bounds2D.aggregate
  boundsAggregate2 = Bounds2D.aggregate2
  boundsIntersection = Bounds2D.intersection
  boundsDiameter = Bounds2D.diameter
  boundsTransformBy = Bounds2D.transformBy

instance BoundsExists 3 Meters space where
  boundsConstant = Bounds3D.constant
  boundsContains = Bounds3D.contains
  boundsHull = Bounds3D.hull
  boundsHull2 = Bounds3D.hull2
  boundsAggregate = Bounds3D.aggregate
  boundsAggregate2 = Bounds3D.aggregate2
  boundsIntersection = Bounds3D.intersection
  boundsDiameter = Bounds3D.diameter
  boundsTransformBy = Bounds3D.transformBy

class VectorTransformExists dimension space where
  vectorTransformAsAffine ::
    VectorTransform dimension tag space ->
    VectorTransform dimension Transform.Tag.Affine space

instance VectorTransformExists 1 Void where
  vectorTransformAsAffine = Data.Coerce.coerce

instance VectorTransformExists 2 Void where
  vectorTransformAsAffine = VectorTransform2D.asAffine

instance VectorTransformExists 3 space where
  vectorTransformAsAffine = VectorTransform3D.asAffine

class TransformExists dimension units space where
  transformVectorTransform ::
    Transform dimension tag units space ->
    VectorTransform dimension tag space
  transformAsAffine ::
    Transform dimension tag units space ->
    Transform dimension Transform.Tag.Affine units space
  transformUniformScale :: Transform dimension tag units space -> Maybe Number

instance TransformExists 2 units Void where
  transformVectorTransform = Transform2D.vectorTransform
  transformAsAffine = Transform2D.asAffine
  transformUniformScale = Transform2D.uniformScale

instance TransformExists 3 Meters space where
  transformVectorTransform = Transform3D.vectorTransform
  transformAsAffine = Transform3D.asAffine
  transformUniformScale = Transform3D.uniformScale
