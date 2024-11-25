module OpenSolid.API (classes, functions) where

import Curve1d (Curve1d)
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
  , rangeUnitless
  , rangeMeters
  , vector2d
  , vector2dUnitless
  , vector2dMeters
  , direction2d
  , point2d
  , point2dUnitless
  , point2dMeters
  , curve1d
  , curve1dUnitless
  , curve1dMeters
  ]

(.:) :: a -> b -> (a, b)
(.:) = (,)

infixr 0 .:

(.|) :: a -> b -> (a, List b)
(.|) a b = (a, [b])

infixr 0 .|

range :: Class
range =
  Class.abstract "Range" $
    [ "unit" .| S0 N Range.unit
    , "constant"
        .: [ S1 N "value" (Range.constant @Unitless)
           , S1 N "value" (Range.constant @Meters)
           ]
    , "from endpoints"
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

rangeUnitless :: Class
rangeUnitless =
  Class.concrete "Range_Unitless" $
    rangeMemberFunctions F

rangeMeters :: Class
rangeMeters =
  Class.concrete "Range_Meters" $
    rangeMemberFunctions L

rangeMemberFunctions ::
  (FFI (Qty units), FFI (Range units)) =>
  Constraint (Tolerance units) ->
  List (Text, List (MemberFunction (Range units)))
rangeMemberFunctions _ =
  [ "endpoints" .| M0 N Range.endpoints
  , "intersection" .| M1 N "other" Range.intersection
  ]

vector2d :: Class
vector2d = do
  Class.abstract "Vector2d" $
    [ "zero" .| S0 N (Vector2d.zero @Space @Meters)
    , "unit" .| S1 N "direction" (Vector2d.unit @Space)
    , "xy"
        .: [ S2 N "x component" "y component" (Vector2d.xy @Space @Unitless)
           , S2 N "x component" "y component" (Vector2d.xy @Space @Meters)
           ]
    , "x"
        .: [ S1 N "x component" (Vector2d.x @Space @Unitless)
           , S1 N "x component" (Vector2d.x @Space @Meters)
           ]
    , "y"
        .: [ S1 N "y component" (Vector2d.y @Space @Unitless)
           , S1 N "y component" (Vector2d.y @Space @Meters)
           ]
    , "from components"
        .: [ S1 N "components" (Vector2d.fromComponents @Space @Unitless)
           , S1 N "components" (Vector2d.fromComponents @Space @Meters)
           ]
    ]

vector2dUnitless :: Class
vector2dUnitless =
  Class.concrete "Vector2d_Unitless" $
    vector2dMemberFunctions F

vector2dMeters :: Class
vector2dMeters =
  Class.concrete "Vector2d_Meters" $
    vector2dMemberFunctions L

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
    , constructors = []
    , staticFunctions =
        [ "x" .| S0 N Direction2d.x
        , "y" .| S0 N Direction2d.y
        , "positive x" .| S0 N Direction2d.positiveX
        , "positive y" .| S0 N Direction2d.positiveY
        , "negative x" .| S0 N Direction2d.negativeX
        , "negative y" .| S0 N Direction2d.negativeY
        , "from angle" .| S1 N "angle" Direction2d.fromAngle
        ]
    , memberFunctions =
        [ "to angle" .| M0 N Direction2d.toAngle
        ]
    }

point2d :: Class
point2d =
  Class.abstract "Point2d" $
    [ "origin" .| S0 N (Point2d.origin @Space @Meters)
    , "xy"
        .: [ S2 N "x coordinate" "y coordinate" (Point2d.xy @Space @Unitless)
           , S2 N "x coordinate" "y coordinate" (Point2d.xy @Space @Meters)
           ]
    , "x"
        .: [ S1 N "x coordinate" (Point2d.x @Space @Unitless)
           , S1 N "x coordinate" (Point2d.x @Space @Meters)
           ]
    , "y"
        .: [ S1 N "y coordinate" (Point2d.y @Space @Unitless)
           , S1 N "y coordinate" (Point2d.y @Space @Meters)
           ]
    , "from coordinates"
        .: [ S1 N "coordinates" (Point2d.fromCoordinates @Space @Unitless)
           , S1 N "coordinates" (Point2d.fromCoordinates @Space @Meters)
           ]
    ]

point2dUnitless :: Class
point2dUnitless =
  Class.concrete "Point2d_Unitless" $
    point2dMemberFunctions F

point2dMeters :: Class
point2dMeters =
  Class.concrete "Point2d_Meters" $
    point2dMemberFunctions L

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

curve1d :: Class
curve1d =
  Class.abstract
    "Curve1d"
    [ "t" .| S0 N Curve1d.parameter
    ]

curve1dUnitless :: Class
curve1dUnitless =
  Class.concrete "Curve1d_Unitless" $
    ("squared" .| M0 N (Curve1d.squared @Unitless))
      : curve1dMemberFunctions F

curve1dMeters :: Class
curve1dMeters =
  Class.concrete "Curve1d_Meters" $
    curve1dMemberFunctions L

curve1dMemberFunctions ::
  forall units.
  (FFI (Qty units), FFI (Curve1d units)) =>
  Constraint (Tolerance units) ->
  List (Text, List (MemberFunction (Curve1d units)))
curve1dMemberFunctions _ =
  [ "evaluate" .| M1 N "parameter value" (\t curve -> Curve1d.evaluate curve t)
  ]

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
classFunctionPairs (Class name constructors staticFunctions memberFunctions) =
  List.map (prefixWith (name + "__")) $
    List.concat
      [ List.map constructorPair constructors
      , List.collect staticFunctionPairs staticFunctions
      , List.collect memberFunctionPairs memberFunctions
      ]
