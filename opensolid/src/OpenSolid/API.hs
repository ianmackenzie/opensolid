module OpenSolid.API (classes, functions) where

import Angle qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
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
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified

data Space

----- API DEFINITION -----

classes :: List Class
classes =
  List.concat
    [ range
    , vector2d
    , direction2d
    , point2d
    , curve1d
    ]

range :: List Class
range =
  [ Class.unitless "Range"
      |> Class.withStaticFunctions
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
  , Class.withUnits "Range" "Unitless"
      |> Class.withMemberFunctions @(Range Unitless)
        [ "Endpoints" .| m0 Range.endpoints
        , "Intersection" .| m1 "Other" Range.intersection
        ]
  , Class.withUnits "Range" "Meters"
      |> Class.withMemberFunctions @(Range Meters)
        [ "Endpoints" .| m0 Range.endpoints
        , "Intersection" .| m1 "Other" Range.intersection
        ]
  ]

vector2d :: List Class
vector2d =
  [ Class.unitless "Vector2d"
      |> Class.withStaticFunctions
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
  , Class.withUnits "Vector2d" "Unitless"
      |> Class.withMemberFunctions @(Vector2d (Space @ Unitless))
        [ "Components" .| m0 Vector2d.components
        , "Direction" .| m0U Vector2d.direction
        ]
  , Class.withUnits "Vector2d" "Meters"
      |> Class.withMemberFunctions @(Vector2d (Space @ Meters))
        [ "Components" .| m0 Vector2d.components
        , "Direction" .| m0M Vector2d.direction
        ]
  ]

direction2d :: List Class
direction2d =
  [ Class.unitless "Direction2d"
      |> Class.withStaticFunctions
        [ "X" .| s0 Direction2d.x
        , "Y" .| s0 Direction2d.y
        , "Positive X" .| s0 Direction2d.positiveX
        , "Positive Y" .| s0 Direction2d.positiveY
        , "Negative X" .| s0 Direction2d.negativeX
        , "Negative Y" .| s0 Direction2d.negativeY
        , "From Angle" .| s1 "Angle" Direction2d.fromAngle
        ]
      |> Class.withMemberFunctions
        [ "To Angle" .| m0 Direction2d.toAngle
        ]
  ]

point2d :: List Class
point2d =
  [ Class.unitless "Point2d"
      |> Class.withStaticFunctions
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
  , Class.withUnits "Point2d" "Unitless"
      |> Class.withMemberFunctions @(Point2d (Space @ Unitless))
        [ "Coordinates" .| m0 Point2d.coordinates
        , "Distance To" .| m1 "Other" Point2d.distanceFrom
        , "Midpoint" .| m1 "Other" Point2d.midpoint
        ]
  , Class.withUnits "Point2d" "Meters"
      |> Class.withMemberFunctions @(Point2d (Space @ Meters))
        [ "Coordinates" .| m0 Point2d.coordinates
        , "Distance To" .| m1 "Other" Point2d.distanceFrom
        , "Midpoint" .| m1 "Other" Point2d.midpoint
        ]
  ]

curve1d :: List Class
curve1d =
  [ Class.unitless "Curve1d"
      |> Class.withStaticFunctions
        [ "T" .| s0 Curve1d.parameter
        , "Sin"
            .: [ s1 "Curve" Curve1d.sin
               , s1 "Curve" (\(floatCurve :: Curve1d Unitless) -> Curve1d.sin (floatCurve * Angle.radian))
               ]
        , "Cos"
            .: [ s1 "Curve" Curve1d.cos
               , s1 "Curve" (\(floatCurve :: Curve1d Unitless) -> Curve1d.cos (floatCurve * Angle.radian))
               ]
        , "Sqrt"
            .: [ s1 "Curve" (Curve1d.sqrt @Unitless)
               ]
        ]
      |> Class.withNestedClasses
        [ Class.nestedUnitless "Curve1d" "Root"
            |> Class.withMemberFunctions @Curve1d.Root
              [ "Value" .| m0 Curve1d.Root.value
              , "Order" .| m0 Curve1d.Root.order
              , "Sign" .| m0 (\root -> 1 * Curve1d.Root.sign root) -- TODO return as enum?
              ]
        ]
  , Class.withUnits "Curve1d" "Unitless"
      |> Class.withMemberFunctions @(Curve1d Unitless)
        [ "Squared" .| m0 Curve1d.squared
        , "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
        , "Zeros" .| m0U Curve1d.zeros
        ]
  , Class.withUnits "Curve1d" "Radians"
      |> Class.withMemberFunctions @(Curve1d Radians)
        [ "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
        , "Zeros" .| m0R Curve1d.zeros
        ]
  , Class.withUnits "Curve1d" "Meters"
      |> Class.withMemberFunctions @(Curve1d Meters)
        [ "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
        , "Zeros" .| m0M Curve1d.zeros
        ]
  ]

----- HELPER OPERATORS / FUNCTIONS -----

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

m0R :: (FFI value, FFI result) => (Tolerance Radians => value -> result) -> MemberFunction value
m0R function = MemberFunction0R function

m0M :: (FFI value, FFI result) => (Tolerance Meters => value -> result) -> MemberFunction value
m0M function = MemberFunction0M function

m1 :: (FFI a, FFI value, FFI result) => Text -> (a -> value -> result) -> MemberFunction value
m1 arg1 function = MemberFunction1 (Name.parse arg1) function

----- FUNCTION COLLECTION -----

type ForeignFunction = Ptr () -> Ptr () -> IO ()

functions :: List (Text, ForeignFunction)
functions = List.collect classFunctionPairs classes

staticFunctionPair :: FFI.Id -> Name -> StaticFunction -> (Text, ForeignFunction)
staticFunctionPair classId functionName staticFunction =
  ( StaticFunction.ffiName classId functionName staticFunction
  , StaticFunction.invoke staticFunction
  )

staticFunctionPairs :: FFI.Id -> (Name, List StaticFunction) -> List (Text, ForeignFunction)
staticFunctionPairs classId (functionName, overloads) =
  List.map (staticFunctionPair classId functionName) overloads

memberFunctionPair :: FFI.Id -> Name -> MemberFunction value -> (Text, ForeignFunction)
memberFunctionPair classId functionName memberFunction =
  ( MemberFunction.ffiName classId functionName memberFunction
  , MemberFunction.invoke memberFunction
  )

memberFunctionPairs :: FFI.Id -> (Name, List (MemberFunction value)) -> List (Text, ForeignFunction)
memberFunctionPairs classId (functionName, overloads) =
  List.map (memberFunctionPair classId functionName) overloads

classFunctionPairs :: Class -> List (Text, Ptr () -> Ptr () -> IO ())
classFunctionPairs (Class id staticFunctions memberFunctions nestedClasses) = do
  List.concat
    [ List.collect (staticFunctionPairs id) staticFunctions
    , List.collect (memberFunctionPairs id) memberFunctions
    , List.collect classFunctionPairs nestedClasses
    ]
