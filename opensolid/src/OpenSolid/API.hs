module OpenSolid.API (classes, functions) where

import Angle qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Data.Proxy (Proxy (Proxy))
import Direction2d (Direction2d)
import Direction2d qualified
import Foreign (Ptr)
import Length (Length)
import List qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.Class (Class (..))
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.NegationOperator qualified as NegationOperator
import OpenSolid.API.PostOperator (PostOperator (PostOperator))
import OpenSolid.API.PostOperator qualified as PostOperator
import OpenSolid.API.PreOperator (PreOperator (PreOperator))
import OpenSolid.API.PreOperator qualified as PreOperator
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

data AbstractRange

instance FFI AbstractRange where
  representation = FFI.abstractClassRepresentation "Range"

range :: List Class
range =
  [ Class
      { id = classId @AbstractRange Proxy
      , staticFunctions =
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
      , memberFunctions = []
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Range Unitless) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Endpoints" .| m0 Range.endpoints
          , "Intersection" .| m1 "Other" Range.intersection
          ]
      , negationFunction = Just negate
      , preOperators =
          [ preAdd
              [ PreOperator ((+) @Float @(Range Unitless))
              ]
          , preSub
              [ PreOperator ((-) @Float @(Range Unitless))
              ]
          , preMul
              [ PreOperator ((*) @Float @(Range Unitless))
              , PreOperator ((*) @Length @(Range Unitless))
              ]
          , preDiv
              [ PreOperator ((/) @Float @(Range Unitless))
              ]
          ]
      , postOperators =
          [ postAdd
              [ PostOperator ((+) @(Range Unitless) @Float)
              , PostOperator ((+) @(Range Unitless) @(Range Unitless))
              ]
          , postSub
              [ PostOperator ((-) @(Range Unitless) @Float)
              , PostOperator ((-) @(Range Unitless) @(Range Unitless))
              ]
          , postMul
              [ PostOperator ((*) @(Range Unitless) @Float)
              , PostOperator ((*) @(Range Unitless) @(Range Unitless))
              , PostOperator ((*) @(Range Unitless) @Length)
              ]
          , postDiv
              [ PostOperator ((/) @(Range Unitless) @Float)
              , PostOperator ((/) @(Range Unitless) @(Range Unitless))
              ]
          ]
      , nestedClasses = []
      }
  , Class
      { id = classId @(Range Meters) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Endpoints" .| m0 Range.endpoints
          , "Intersection" .| m1 "Other" Range.intersection
          ]
      , negationFunction = Just negate
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  ]

data AbstractVector2d

instance FFI AbstractVector2d where
  representation = FFI.abstractClassRepresentation "Vector2d"

vector2d :: List Class
vector2d =
  [ Class
      { id = classId @AbstractVector2d Proxy
      , staticFunctions =
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
      , memberFunctions = []
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Vector2d (Space @ Unitless)) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Components" .| m0 Vector2d.components
          , "Direction" .| m0U Vector2d.direction
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Vector2d (Space @ Meters)) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Components" .| m0 Vector2d.components
          , "Direction" .| m0M Vector2d.direction
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  ]

direction2d :: List Class
direction2d =
  [ Class
      { id = classId @(Direction2d Space) Proxy
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
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  ]

data AbstractPoint2d

instance FFI AbstractPoint2d where
  representation = FFI.abstractClassRepresentation "Point2d"

point2d :: List Class
point2d =
  [ Class
      { id = classId @AbstractPoint2d Proxy
      , staticFunctions =
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
      , memberFunctions = []
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Point2d (Space @ Unitless)) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Coordinates" .| m0 Point2d.coordinates
          , "Distance To" .| m1 "Other" Point2d.distanceFrom
          , "Midpoint" .| m1 "Other" Point2d.midpoint
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Point2d (Space @ Meters)) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Coordinates" .| m0 Point2d.coordinates
          , "Distance To" .| m1 "Other" Point2d.distanceFrom
          , "Midpoint" .| m1 "Other" Point2d.midpoint
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  ]

data AbstractCurve1d

instance FFI AbstractCurve1d where
  representation = FFI.abstractClassRepresentation "Curve1d"

curve1d :: List Class
curve1d =
  [ Class
      { id = classId @AbstractCurve1d Proxy
      , staticFunctions =
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
      , memberFunctions = []
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses =
          [ Class
              { id = classId @Curve1d.Root Proxy
              , staticFunctions = []
              , memberFunctions =
                  [ "Value" .| m0 Curve1d.Root.value
                  , "Order" .| m0 Curve1d.Root.order
                  , "Sign" .| m0 (\root -> 1 * Curve1d.Root.sign root) -- TODO return as enum?
                  ]
              , negationFunction = Nothing
              , preOperators = []
              , postOperators = []
              , nestedClasses = []
              }
          ]
      }
  , Class
      { id = classId @(Curve1d Unitless) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Squared" .| m0 Curve1d.squared
          , "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
          , "Zeros" .| m0U Curve1d.zeros
          ]
      , negationFunction = Just negate
      , preOperators =
          [ preAdd
              [ PreOperator ((+) @Float @(Curve1d Unitless))
              ]
          , preSub
              [ PreOperator ((-) @Float @(Curve1d Unitless))
              ]
          , preMul
              [ PreOperator ((*) @Float @(Curve1d Unitless))
              ]
          , preDiv
              [ PreOperator ((/) @Float @(Curve1d Unitless))
              ]
          ]
      , postOperators =
          [ postAdd
              [ PostOperator ((+) @(Curve1d Unitless) @Float)
              , PostOperator ((+) @(Curve1d Unitless) @(Curve1d Unitless))
              ]
          , postSub
              [ PostOperator ((-) @(Curve1d Unitless) @Float)
              , PostOperator ((-) @(Curve1d Unitless) @(Curve1d Unitless))
              ]
          , postMul
              [ PostOperator ((*) @(Curve1d Unitless) @Float)
              , PostOperator ((*) @(Curve1d Unitless) @(Curve1d Unitless))
              ]
          , postDiv
              [ PostOperator ((/) @(Curve1d Unitless) @Float)
              , PostOperator ((/) @(Curve1d Unitless) @(Curve1d Unitless))
              ]
          ]
      , nestedClasses = []
      }
  , Class
      { id = classId @(Curve1d Radians) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
          , "Zeros" .| m0R Curve1d.zeros
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  , Class
      { id = classId @(Curve1d Meters) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Evaluate" .| m1 "Parameter Value" (\t curve -> Curve1d.evaluate curve t)
          , "Zeros" .| m0M Curve1d.zeros
          ]
      , negationFunction = Nothing
      , preOperators = []
      , postOperators = []
      , nestedClasses = []
      }
  ]

----- HELPER OPERATORS / FUNCTIONS -----

classId :: forall a. FFI a => Proxy a -> FFI.Id a
classId proxy = case FFI.typeOf proxy of
  FFI.Class (FFI.Id Proxy names maybeUnits) -> FFI.Id Proxy names maybeUnits
  _ -> internalError "Attempt to get class ID of a non-class"

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

preAdd :: List (PreOperator value) -> (BinaryOperator.Id, List (PreOperator value))
preAdd overloads = (BinaryOperator.Add, overloads)

postAdd :: List (PostOperator value) -> (BinaryOperator.Id, List (PostOperator value))
postAdd overloads = (BinaryOperator.Add, overloads)

preSub :: List (PreOperator value) -> (BinaryOperator.Id, List (PreOperator value))
preSub overloads = (BinaryOperator.Sub, overloads)

postSub :: List (PostOperator value) -> (BinaryOperator.Id, List (PostOperator value))
postSub overloads = (BinaryOperator.Sub, overloads)

preMul :: List (PreOperator value) -> (BinaryOperator.Id, List (PreOperator value))
preMul overloads = (BinaryOperator.Mul, overloads)

postMul :: List (PostOperator value) -> (BinaryOperator.Id, List (PostOperator value))
postMul overloads = (BinaryOperator.Mul, overloads)

preDiv :: List (PreOperator value) -> (BinaryOperator.Id, List (PreOperator value))
preDiv overloads = (BinaryOperator.Div, overloads)

postDiv :: List (PostOperator value) -> (BinaryOperator.Id, List (PostOperator value))
postDiv overloads = (BinaryOperator.Div, overloads)

----- FUNCTION COLLECTION -----

type ForeignFunction = Ptr () -> Ptr () -> IO ()

functions :: List (Text, ForeignFunction)
functions = List.collect classFunctionPairs classes

staticFunctionPair :: FFI.Id a -> Name -> StaticFunction -> (Text, ForeignFunction)
staticFunctionPair classId_ functionName staticFunction =
  ( StaticFunction.ffiName classId_ functionName staticFunction
  , StaticFunction.invoke staticFunction
  )

staticFunctionPairs :: FFI.Id a -> (Name, List StaticFunction) -> List (Text, ForeignFunction)
staticFunctionPairs classId_ (functionName, overloads) =
  List.map (staticFunctionPair classId_ functionName) overloads

memberFunctionPair :: FFI.Id value -> Name -> MemberFunction value -> (Text, ForeignFunction)
memberFunctionPair classId_ functionName memberFunction =
  ( MemberFunction.ffiName classId_ functionName memberFunction
  , MemberFunction.invoke memberFunction
  )

memberFunctionPairs :: FFI.Id value -> (Name, List (MemberFunction value)) -> List (Text, ForeignFunction)
memberFunctionPairs classId_ (functionName, overloads) =
  List.map (memberFunctionPair classId_ functionName) overloads

negationOperatorPair :: FFI value => FFI.Id value -> Maybe (value -> value) -> List (Text, ForeignFunction)
negationOperatorPair classId_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction ->
    [(NegationOperator.ffiName classId_, NegationOperator.invoke negationFunction)]

preOperatorPair :: FFI.Id value -> BinaryOperator.Id -> PreOperator value -> (Text, ForeignFunction)
preOperatorPair classId_ operatorId operator =
  ( PreOperator.ffiName classId_ operatorId operator
  , PreOperator.invoke operator
  )

preOperatorPairs ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PreOperator value)) ->
  List (Text, ForeignFunction)
preOperatorPairs classId_ (operatorId, overloads) =
  List.map (preOperatorPair classId_ operatorId) overloads

postOperatorPair :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> (Text, ForeignFunction)
postOperatorPair classId_ operatorId operator =
  ( PostOperator.ffiName classId_ operatorId operator
  , PostOperator.invoke operator
  )

postOperatorPairs ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PostOperator value)) ->
  List (Text, ForeignFunction)
postOperatorPairs classId_ (operatorId, overloads) =
  List.map (postOperatorPair classId_ operatorId) overloads

classFunctionPairs :: Class -> List (Text, Ptr () -> Ptr () -> IO ())
classFunctionPairs (Class classId_ staticFunctions memberFunctions maybeNegationOperator preOperators postOperators nestedClasses) =
  List.concat
    [ List.collect (staticFunctionPairs classId_) staticFunctions
    , List.collect (memberFunctionPairs classId_) memberFunctions
    , negationOperatorPair classId_ maybeNegationOperator
    , List.collect (preOperatorPairs classId_) preOperators
    , List.collect (postOperatorPairs classId_) postOperators
    , List.collect classFunctionPairs nestedClasses
    ]
