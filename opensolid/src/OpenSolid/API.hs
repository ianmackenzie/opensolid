module OpenSolid.API (classes, functions) where

import Curve1d qualified
import Direction2d qualified
import Foreign (Ptr)
import List qualified
import OpenSolid
import OpenSolid.API.Class (Class (..))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.Class.Constructor (Constructor (..))
import OpenSolid.API.Class.Constructor qualified as Constructor
import OpenSolid.API.Class.MemberFunction (MemberFunction (..))
import OpenSolid.API.Class.MemberFunction qualified as MemberFunction
import OpenSolid.API.Class.StaticFunction (StaticFunction (..))
import OpenSolid.API.Class.StaticFunction qualified as StaticFunction
import OpenSolid.API.Constraint (Constraint (..))
import OpenSolid.FFI (FFI)
import Pair qualified
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified

data Space

classes :: List Class
classes =
  [ range
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

range :: Class
range =
  Class.outer
    "Range"
    [ "unit" .| S0 N Range.unit
    , "constant"
        .: [ S1 N "value" (Range.constant @Unitless)
           , S1 N "value" (Range.constant @Meters)
           ]
    , "from_endpoints"
        .: [ S2 N "a" "b" (Range.from @Unitless)
           , S2 N "a" "b" (Range.from @Meters)
           ]
    , "aggregate"
        .: [ S2 N "a" "b" (Range.aggregate2 @Unitless)
           , S3 N "a" "b" "c" (Range.aggregate3 @Unitless)
           , S2 N "a" "b" (Range.aggregate2 @Meters)
           , S3 N "a" "b" "c" (Range.aggregate3 @Meters)
           ]
    ]
    [ rangeUnitless
    , rangeMeters
    ]

rangeUnitless :: Class
rangeUnitless =
  Class
    { name = "Unitless"
    , constructors = []
    , staticFunctions = []
    , memberFunctions = rangeMemberFunctions F
    , nestedClasses = []
    }

rangeMeters :: Class
rangeMeters =
  Class
    { name = "Meters"
    , constructors = []
    , staticFunctions = []
    , memberFunctions = rangeMemberFunctions L
    , nestedClasses = []
    }

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
    , nestedClasses = []
    }

vector2d :: Class
vector2d =
  Class
    { name = "Vector2d"
    , constructors = vector2dConstructors L
    , staticFunctions = vector2dStaticFunctions L
    , memberFunctions = vector2dMemberFunctions L
    , nestedClasses = []
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
    , nestedClasses = []
    }

point2f :: Class
point2f =
  Class "Point2f" (point2dConstructors F) (point2dStaticFunctions F) (point2dMemberFunctions F) []

point2d :: Class
point2d =
  Class "Point2d" (point2dConstructors L) (point2dStaticFunctions L) (point2dMemberFunctions L) []

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
    , nestedClasses = []
    }

type ForeignFunction = Ptr () -> Ptr () -> IO ()

functions :: List (Text, ForeignFunction)
functions =
  List.map (Pair.mapFirst ("opensolid__" +)) $
    List.collect classFunctionPairs classes

constructorPair :: Constructor value -> (Text, ForeignFunction)
constructorPair constructor =
  (Constructor.ffiName constructor, Constructor.invoke constructor)

staticFunctionPair :: Text -> StaticFunction -> (Text, ForeignFunction)
staticFunctionPair functionName staticFunction =
  (StaticFunction.ffiName functionName staticFunction, StaticFunction.invoke staticFunction)

staticFunctionPairs :: (Text, List StaticFunction) -> List (Text, ForeignFunction)
staticFunctionPairs (functionName, overloads) =
  List.map (staticFunctionPair functionName) overloads

memberFunctionPair :: Text -> MemberFunction value -> (Text, ForeignFunction)
memberFunctionPair functionName memberFunction =
  (MemberFunction.ffiName functionName memberFunction, MemberFunction.invoke memberFunction)

memberFunctionPairs :: (Text, List (MemberFunction value)) -> List (Text, ForeignFunction)
memberFunctionPairs (functionName, overloads) =
  List.map (memberFunctionPair functionName) overloads

prefixWith :: Text -> (Text, a) -> (Text, a)
prefixWith prefix = Pair.mapFirst (prefix +)

classFunctionPairs :: Class -> List (Text, Ptr () -> Ptr () -> IO ())
classFunctionPairs (Class name constructors staticFunctions memberFunctions nestedClasses) =
  List.concat
    [ List.map (prefixWith (name + "__")) $
        List.concat
          [ List.map constructorPair constructors
          , List.collect staticFunctionPairs staticFunctions
          , List.collect memberFunctionPairs memberFunctions
          ]
    , List.map (prefixWith (name + "_")) (List.collect classFunctionPairs nestedClasses)
    ]
