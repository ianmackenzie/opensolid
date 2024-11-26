module OpenSolid.API (classes, functions) where

import Curve1d (Curve1d)
import Curve1d qualified
import Direction2d qualified
import Foreign (Ptr)
import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.API.Class (Class (..))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.Class.MemberFunction (MemberFunction (..))
import OpenSolid.API.Class.MemberFunction qualified as MemberFunction
import OpenSolid.API.Class.StaticFunction (StaticFunction (..))
import OpenSolid.API.Class.StaticFunction qualified as StaticFunction
import OpenSolid.API.Constraint (Constraint (..))
import OpenSolid.API.Name qualified as Name
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
  List.concat
    [ range
    , vector2d
    , direction2d
    , point2d
    , curve1d
    ]

----- HELPER OPERATORS -----

(.:) :: a -> b -> (a, b)
(.:) = (,)

infixr 0 .:

(.|) :: a -> b -> (a, List b)
(.|) a b = (a, [b])

infixr 0 .|

----- API DEFINITION -----

range :: List Class
range =
  [ Class.abstract "Range" $
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
  , Class.concrete @(Range Unitless) "Range" "Unitless" $
      [ "endpoints" .| M0 N Range.endpoints
      , "intersection" .| M1 N "other" Range.intersection
      ]
  , Class.concrete @(Range Meters) "Range" "Meters" $
      [ "endpoints" .| M0 N Range.endpoints
      , "intersection" .| M1 N "other" Range.intersection
      ]
  ]

vector2d :: List Class
vector2d =
  [ Class.abstract "Vector2d" $
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
  , Class.concrete @(Vector2d (Space @ Unitless)) "Vector2d" "Unitless" $
      [ "components" .| M0 N Vector2d.components
      , "direction" .| M0 F Vector2d.direction
      ]
  , Class.concrete @(Vector2d (Space @ Meters)) "Vector2d" "Meters" $
      [ "components" .| M0 N Vector2d.components
      , "direction" .| M0 L Vector2d.direction
      ]
  ]

direction2d :: List Class
direction2d =
  [ Class
      { name = Name.parse "Direction2d"
      , units = Nothing
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
  ]

point2d :: List Class
point2d =
  [ Class.abstract "Point2d" $
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
  , Class.concrete @(Point2d (Space @ Unitless)) "Point2d" "Unitless" $
      [ "coordinates" .| M0 N Point2d.coordinates
      , "distance to" .| M1 N "other" Point2d.distanceFrom
      , "midpoint" .| M1 N "other" Point2d.midpoint
      ]
  , Class.concrete @(Point2d (Space @ Meters)) "Point2d" "Meters" $
      [ "coordinates" .| M0 N Point2d.coordinates
      , "distance to" .| M1 N "other" Point2d.distanceFrom
      , "midpoint" .| M1 N "other" Point2d.midpoint
      ]
  ]

curve1d :: List Class
curve1d =
  [ Class.abstract "Curve1d" $
      [ "t" .| S0 N Curve1d.parameter
      ]
  , Class.concrete @(Curve1d Unitless) "Curve1d" "Unitless" $
      [ "squared" .| M0 N Curve1d.squared
      , "evaluate" .| M1 N "parameter value" (\t curve -> Curve1d.evaluate curve t)
      ]
  , Class.concrete @(Curve1d Meters) "Curve1d" "Meters" $
      [ "evaluate" .| M1 N "parameter value" (\t curve -> Curve1d.evaluate curve t)
      ]
  ]

----- FUNCTION COLLECTION -----

type ForeignFunction = Ptr () -> Ptr () -> IO ()

functions :: List (Text, ForeignFunction)
functions =
  List.map (Pair.mapFirst ("opensolid_" +)) $
    List.collect classFunctionPairs classes

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
classFunctionPairs (Class name units staticFunctions memberFunctions) = do
  List.map (prefixWith (Name.pascalCase name + Maybe.map Name.pascalCase units + "_")) $
    List.concat
      [ List.collect staticFunctionPairs staticFunctions
      , List.collect memberFunctionPairs memberFunctions
      ]
