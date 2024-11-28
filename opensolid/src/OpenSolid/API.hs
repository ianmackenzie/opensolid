module OpenSolid.API (classes, functions) where

import Curve1d (Curve1d)
import Curve1d qualified
import Direction2d qualified
import Foreign (Ptr)
import List qualified
import OpenSolid
import OpenSolid.API.Class (Class (..))
import OpenSolid.API.Class qualified as Class
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
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

(.:) :: Text -> List a -> (Name, List a)
(.:) name values = (Name.parse name, values)

infixr 0 .:

(.|) :: Text -> a -> (Name, List a)
(.|) name value = (Name.parse name, [value])

infixr 0 .|

s0 :: FFI a => a -> StaticFunction
s0 value = StaticFunction0 value

s1 :: (FFI a, FFI b) => Text -> (a -> b) -> StaticFunction
s1 arg1 function = StaticFunction1 (Name.parse arg1) function

s2 :: (FFI a, FFI b, FFI c) => Text -> Text -> (a -> b -> c) -> StaticFunction
s2 arg1 arg2 function = StaticFunction2 (Name.parse arg1) (Name.parse arg2) function

s3 :: (FFI a, FFI b, FFI c, FFI d) => Text -> Text -> Text -> (a -> b -> c -> d) -> StaticFunction
s3 arg1 arg2 arg3 function =
  StaticFunction3 (Name.parse arg1) (Name.parse arg2) (Name.parse arg3) function

m0 :: (FFI value, FFI result) => (value -> result) -> MemberFunction value
m0 function = MemberFunction0 function

m0U :: (FFI value, FFI result) => (Tolerance Unitless => value -> result) -> MemberFunction value
m0U function = MemberFunction0U function

m0M :: (FFI value, FFI result) => (Tolerance Meters => value -> result) -> MemberFunction value
m0M function = MemberFunction0M function

m1 :: (FFI a, FFI value, FFI result) => Text -> (a -> value -> result) -> MemberFunction value
m1 arg1 function = MemberFunction1 (Name.parse arg1) function

----- API DEFINITION -----

range :: List Class
range =
  [ Class.abstract "Range" $
      [ "Unit" .| s0 Range.unit
      , "Constant"
          .: [ s1 "Value" (Range.constant @Unitless)
             , s1 "Value" (Range.constant @Meters)
             ]
      , "From Endpoints"
          .: [ s2 "A" "B" (Range.from @Unitless)
             , s2 "A" "B" (Range.from @Meters)
             ]
      , "Aggregate"
          .: [ s2 "A" "B" (Range.aggregate2 @Unitless)
             , s3 "A" "B" "C" (Range.aggregate3 @Unitless)
             , s2 "A" "B" (Range.aggregate2 @Meters)
             , s3 "A" "B" "C" (Range.aggregate3 @Meters)
             ]
      ]
  , Class.concrete @(Range Unitless) "Range" "Unitless" $
      [ "Endpoints" .| m0 Range.endpoints
      , "Intersection" .| m1 "Other" Range.intersection
      ]
  , Class.concrete @(Range Meters) "Range" "Meters" $
      [ "Endpoints" .| m0 Range.endpoints
      , "Intersection" .| m1 "Other" Range.intersection
      ]
  ]

vector2d :: List Class
vector2d =
  [ Class.abstract "Vector2d" $
      [ "Zero" .| s0 (Vector2d.zero @Space @Meters)
      , "Unit" .| s1 "Direction" (Vector2d.unit @Space)
      , "XY"
          .: [ s2 "X Component" "Y Component" (Vector2d.xy @Space @Unitless)
             , s2 "X Component" "Y Component" (Vector2d.xy @Space @Meters)
             ]
      , "X"
          .: [ s1 "X Component" (Vector2d.x @Space @Unitless)
             , s1 "X Component" (Vector2d.x @Space @Meters)
             ]
      , "Y"
          .: [ s1 "Y Component" (Vector2d.y @Space @Unitless)
             , s1 "Y Component" (Vector2d.y @Space @Meters)
             ]
      , "From Components"
          .: [ s1 "Components" (Vector2d.fromComponents @Space @Unitless)
             , s1 "Components" (Vector2d.fromComponents @Space @Meters)
             ]
      ]
  , Class.concrete @(Vector2d (Space @ Unitless)) "Vector2d" "Unitless" $
      [ "Components" .| m0 Vector2d.components
      , "Direction" .| m0U Vector2d.direction
      ]
  , Class.concrete @(Vector2d (Space @ Meters)) "Vector2d" "Meters" $
      [ "Components" .| m0 Vector2d.components
      , "Direction" .| m0M Vector2d.direction
      ]
  ]

direction2d :: List Class
direction2d =
  [ Class
      { name = Name.parse "Direction2d"
      , units = Nothing
      , staticFunctions =
          [ "X" .| s0 Direction2d.x
          , "Y" .| s0 Direction2d.y
          , "Positive X" .| s0 Direction2d.positiveX
          , "Positive Y" .| s0 Direction2d.positiveY
          , "Negative X" .| s0 Direction2d.negativeX
          , "Negative Y" .| s0 Direction2d.negativeY
          , "From Angle" .| s1 "Angle" Direction2d.fromAngle
          ]
      , memberFunctions =
          [ "To Angle" .| m0 Direction2d.toAngle
          ]
      }
  ]

point2d :: List Class
point2d =
  [ Class.abstract "Point2d" $
      [ "Origin" .| s0 (Point2d.origin @Space @Meters)
      , "XY"
          .: [ s2 "X Coordinate" "Y Coordinate" (Point2d.xy @Space @Unitless)
             , s2 "X Coordinate" "Y Coordinate" (Point2d.xy @Space @Meters)
             ]
      , "X"
          .: [ s1 "X Coordinate" (Point2d.x @Space @Unitless)
             , s1 "X Coordinate" (Point2d.x @Space @Meters)
             ]
      , "Y"
          .: [ s1 "Y Coordinate" (Point2d.y @Space @Unitless)
             , s1 "Y Coordinate" (Point2d.y @Space @Meters)
             ]
      , "From Coordinates"
          .: [ s1 "Coordinates" (Point2d.fromCoordinates @Space @Unitless)
             , s1 "Coordinates" (Point2d.fromCoordinates @Space @Meters)
             ]
      ]
  , Class.concrete @(Point2d (Space @ Unitless)) "Point2d" "Unitless" $
      [ "Coordinates" .| m0 Point2d.coordinates
      , "Distance To" .| m1 "Other" Point2d.distanceFrom
      , "Midpoint" .| m1 "Other" Point2d.midpoint
      ]
  , Class.concrete @(Point2d (Space @ Meters)) "Point2d" "Meters" $
      [ "Coordinates" .| m0 Point2d.coordinates
      , "Distance To" .| m1 "Other" Point2d.distanceFrom
      , "Midpoint" .| m1 "Other" Point2d.midpoint
      ]
  ]

curve1d :: List Class
curve1d =
  [ Class.abstract "Curve1d" $
      [ "T" .| s0 Curve1d.parameter
      ]
  , Class.concrete @(Curve1d Unitless) "Curve1d" "Unitless" $
      [ "Squared" .| m0 Curve1d.squared
      , "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
      ]
  , Class.concrete @(Curve1d Meters) "Curve1d" "Meters" $
      [ "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
      ]
  ]

----- FUNCTION COLLECTION -----

type ForeignFunction = Ptr () -> Ptr () -> IO ()

functions :: List (Text, ForeignFunction)
functions =
  List.map (Pair.mapFirst ("opensolid_" +)) $
    List.collect classFunctionPairs classes

staticFunctionPair :: Name -> StaticFunction -> (Text, ForeignFunction)
staticFunctionPair functionName staticFunction =
  (StaticFunction.ffiName functionName staticFunction, StaticFunction.invoke staticFunction)

staticFunctionPairs :: (Name, List StaticFunction) -> List (Text, ForeignFunction)
staticFunctionPairs (functionName, overloads) =
  List.map (staticFunctionPair functionName) overloads

memberFunctionPair :: Name -> MemberFunction value -> (Text, ForeignFunction)
memberFunctionPair functionName memberFunction =
  (MemberFunction.ffiName functionName memberFunction, MemberFunction.invoke memberFunction)

memberFunctionPairs :: (Name, List (MemberFunction value)) -> List (Text, ForeignFunction)
memberFunctionPairs (functionName, overloads) =
  List.map (memberFunctionPair functionName) overloads

prefixWith :: Text -> (Text, a) -> (Text, a)
prefixWith prefix = Pair.mapFirst (prefix +)

classFunctionPairs :: Class -> List (Text, Ptr () -> Ptr () -> IO ())
classFunctionPairs (Class baseName maybeUnits staticFunctions memberFunctions) = do
  List.map (prefixWith (FFI.className baseName maybeUnits + "_")) $
    List.concat
      [ List.collect staticFunctionPairs staticFunctions
      , List.collect memberFunctionPairs memberFunctions
      ]
