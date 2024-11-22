module OpenSolid.API (classes) where

import Curve1d qualified
import Direction2d qualified
import OpenSolid
import OpenSolid.API.Class (Class (..), Constraint (..), Constructor (..), MemberFunction (..), StaticFunction (..))
import OpenSolid.FFI (FFI)
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Vector2d (Vector2d)
import Vector2d qualified

data Space

classes :: List Class
classes =
  [ floatRange
  , lengthRange
  , vector2f
  , vector2d
  , direction2d
  , point2f
  , point2d
  , curve1f
  ]

(.:) :: a -> b -> (a, b)
(.:) = (,)

infixr 0 .:

(.|) :: a -> b -> (a, List b)
(.|) a b = (a, [b])

infixr 0 .|

floatRange :: Class
floatRange =
  Class
    { name = "FloatRange"
    , constructors = rangeConstructors F
    , staticFunctions = ("unit", [S0 N Range.unit]) : rangeStaticFunctions F
    , memberFunctions = rangeMemberFunctions F
    }

lengthRange :: Class
lengthRange =
  Class
    { name = "LengthRange"
    , constructors = rangeConstructors L
    , staticFunctions = rangeStaticFunctions L
    , memberFunctions = rangeMemberFunctions L
    }

rangeConstructors ::
  (FFI (Qty units), FFI (Range units)) =>
  Constraint (Tolerance units) ->
  List (Constructor (Range units))
rangeConstructors _ =
  [ C1 N "value" Range.constant
  , C2 N "low" "high" Range.from
  ]

rangeStaticFunctions ::
  forall units.
  (FFI (Qty units), FFI (Range units)) =>
  Constraint (Tolerance units) ->
  List (Text, List StaticFunction)
rangeStaticFunctions _ =
  [ "aggregate"
      .: [ S2 N "first" "second" (Range.aggregate2 @units)
         , S3 N "first" "second" "third" (Range.aggregate3 @units)
         ]
  ]

rangeMemberFunctions ::
  (FFI (Qty units), FFI (Range units)) =>
  Constraint (Tolerance units) ->
  List (Text, List (MemberFunction (Range units)))
rangeMemberFunctions _ =
  [ "endpoints" .| M0 N Range.endpoints
  , "intersection" .| M1 N "other" Range.intersection
  ]

vector2f :: Class
vector2f =
  Class
    { name = "Vector2f"
    , constructors = C1 N "direction" Vector2d.unit : vector2dConstructors F
    , staticFunctions = vector2dStaticFunctions F
    , memberFunctions = vector2dMemberFunctions F
    }

vector2d :: Class
vector2d =
  Class
    { name = "Vector2d"
    , constructors = vector2dConstructors L
    , staticFunctions = vector2dStaticFunctions L
    , memberFunctions = vector2dMemberFunctions L
    }

vector2dConstructors ::
  (FFI (Qty units), FFI (Vector2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Constructor (Vector2d (Space @ units)))
vector2dConstructors _ =
  [ C2 N "x" "y" Vector2d.xy
  , C1 N "components" Vector2d.fromComponents
  ]

vector2dStaticFunctions ::
  forall units.
  (FFI (Qty units), FFI (Vector2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Text, List StaticFunction)
vector2dStaticFunctions _ =
  [ "zero" .| S0 N (Vector2d.zero @Space @units)
  , "x" .| S1 N "x" (Vector2d.x @Space @units)
  , "y" .| S1 N "y" (Vector2d.y @Space @units)
  ]

vector2dMemberFunctions ::
  forall units.
  (FFI (Qty units), FFI (Vector2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Text, List (MemberFunction (Vector2d (Space @ units))))
vector2dMemberFunctions constraint =
  [ "components" .| M0 N Vector2d.components
  , "direction" .| M0 constraint Vector2d.direction
  ]

direction2d :: Class
direction2d =
  Class
    { name = "Direction2d"
    , constructors =
        [ C1 N "angle" Direction2d.fromAngle
        ]
    , staticFunctions =
        [ "x" .| S0 N Direction2d.x
        , "y" .| S0 N Direction2d.y
        , "positive x" .| S0 N Direction2d.positiveX
        , "positive y" .| S0 N Direction2d.positiveY
        , "negative x" .| S0 N Direction2d.negativeX
        , "negative y" .| S0 N Direction2d.negativeY
        ]
    , memberFunctions =
        [ "to angle" .| M0 N Direction2d.toAngle
        ]
    }

point2f :: Class
point2f =
  Class "Point2f" (point2dConstructors F) (point2dStaticFunctions F) (point2dMemberFunctions F)

point2d :: Class
point2d =
  Class "Point2d" (point2dConstructors L) (point2dStaticFunctions L) (point2dMemberFunctions L)

point2dConstructors ::
  (FFI (Qty units), FFI (Point2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Constructor (Point2d (Space @ units)))
point2dConstructors _ =
  [ C2 N "x" "y" Point2d.xy
  , C1 N "coordinates" Point2d.fromCoordinates
  ]

point2dStaticFunctions ::
  forall units.
  (FFI (Qty units), FFI (Point2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Text, List StaticFunction)
point2dStaticFunctions _ =
  [ "origin" .| S0 N (Point2d.origin @Space @units)
  , "x" .| S1 N "x" (Point2d.x @Space @units)
  , "y" .| S1 N "y" (Point2d.y @Space @units)
  ]

point2dMemberFunctions ::
  forall units.
  (FFI (Qty units), FFI (Point2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Text, List (MemberFunction (Point2d (Space @ units))))
point2dMemberFunctions _ =
  [ "coordinates" .| M0 N Point2d.coordinates
  , "distance to" .| M1 N "other" Point2d.distanceFrom
  , "midpoint" .| M1 N "other" Point2d.midpoint
  ]

curve1f :: Class
curve1f =
  Class
    { name = "Curve1f"
    , constructors = []
    , staticFunctions =
        [ "t" .| S0 N Curve1d.parameter
        ]
    , memberFunctions =
        [ "evaluate" .| M1 N "parameter value" (\t curve -> Curve1d.evaluate curve t)
        , "squared" .| M0 N (Curve1d.squared @Unitless)
        ]
    }
