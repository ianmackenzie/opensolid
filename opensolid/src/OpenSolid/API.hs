module OpenSolid.API (Class (..), Function (..), classes, functions) where

import Angle qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Data.Proxy (Proxy (Proxy))
import Direction2d (Direction2d)
import Direction2d qualified
import Foreign (Ptr)
import Length (Length)
import Length qualified
import List qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.Constraint (Constraint)
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
import Pair qualified
import Point2d (Point2d)
import Point2d qualified
import Range (Range)
import Range qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified

data Space

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id value
    , staticFunctions :: List (Name, List StaticFunction)
    , memberFunctions :: List (Name, List (MemberFunction value))
    , negationFunction :: Maybe (value -> value)
    , preOperators :: List (BinaryOperator.Id, List (PreOperator value))
    , postOperators :: List (BinaryOperator.Id, List (PostOperator value))
    , nestedClasses :: List Class
    } ->
    Class

data Function = Function
  { ffiName :: Text
  , constraint :: Maybe Constraint
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Ptr () -> Ptr () -> IO ()
  }

----- API DEFINITION -----

classes :: List Class
classes =
  List.concat
    [ length
    , angle
    , range
    , vector2d
    , direction2d
    , point2d
    , curve1d
    ]

length :: List Class
length =
  [ Class
      { id = classId @Length Proxy
      , staticFunctions =
          [ "Zero" .| s0 Length.zero
          , "Meters" .| s1 "Value" Length.meters
          ]
      , memberFunctions =
          [ "In Meters" .| m0 Length.inMeters
          ]
      , negationFunction = Just negate
      , preOperators =
          [ preMul [PreOperator ((*) @Float @Length)]
          ]
      , postOperators =
          [ postAdd
              [ PostOperator ((+) @Length @Length)
              , PostOperator ((+) @Length @(Range Meters))
              , PostOperator ((+) @Length @(Curve1d Meters))
              ]
          , postSub
              [ PostOperator ((-) @Length @Length)
              , PostOperator ((-) @Length @(Range Meters))
              , PostOperator ((-) @Length @(Curve1d Meters))
              ]
          , postMul
              [ PostOperator ((*) @Length @Float)
              , PostOperator ((*) @Length @(Range Unitless))
              , PostOperator ((*) @Length @(Curve1d Unitless))
              ]
          , postDiv
              [ PostOperator ((/) @Length @Float)
              , PostOperator ((/) @Length @Length)
              , PostOperator ((/) @Length @(Range Unitless))
              , PostOperator ((/) @Length @(Range Meters))
              , PostOperator ((/) @Length @(Curve1d Unitless))
              , PostOperator ((/) @Length @(Curve1d Meters))
              ]
          ]
      , nestedClasses = []
      }
  ]

angle :: List Class
angle =
  [ Class
      { id = classId @Angle Proxy
      , staticFunctions =
          [ "Zero" .| s0 Angle.zero
          , "Radians" .| s1 "Value" Angle.radians
          ]
      , memberFunctions =
          [ "In Radians" .| m0 Angle.inRadians
          ]
      , negationFunction = Just negate
      , preOperators =
          [ preMul [PreOperator ((*) @Float @Angle)]
          ]
      , postOperators =
          [ postAdd
              [ PostOperator ((+) @Angle @Angle)
              , PostOperator ((+) @Angle @(Range Radians))
              , PostOperator ((+) @Angle @(Curve1d Radians))
              ]
          , postSub
              [ PostOperator ((-) @Angle @Angle)
              , PostOperator ((-) @Angle @(Range Radians))
              , PostOperator ((-) @Angle @(Curve1d Radians))
              ]
          , postMul
              [ PostOperator ((*) @Angle @Float)
              , PostOperator ((*) @Angle @(Range Unitless))
              , PostOperator ((*) @Angle @(Curve1d Unitless))
              ]
          , postDiv
              [ PostOperator ((/) @Angle @Float)
              , PostOperator ((/) @Angle @Angle)
              , PostOperator ((/) @Angle @(Range Unitless))
              , PostOperator ((/) @Angle @(Range Radians))
              , PostOperator ((/) @Angle @(Curve1d Unitless))
              , PostOperator ((/) @Angle @(Curve1d Radians))
              ]
          ]
      , nestedClasses = []
      }
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
          [ preAdd [PreOperator ((+) @Float @(Range Unitless))]
          , preSub [PreOperator ((-) @Float @(Range Unitless))]
          , preMul [PreOperator ((*) @Float @(Range Unitless))]
          , preDiv [PreOperator ((/) @Float @(Range Unitless))]
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
              , PostOperator ((*) @(Range Unitless) @Angle)
              ]
          , postDiv
              [ PostOperator ((/) @(Range Unitless) @Float)
              , PostOperator ((/) @(Range Unitless) @(Range Unitless))
              ]
          ]
      , nestedClasses = []
      }
  , Class
      { id = classId @(Range Radians) Proxy
      , staticFunctions = []
      , memberFunctions =
          [ "Endpoints" .| m0 Range.endpoints
          , "Intersection" .| m1 "Other" Range.intersection
          ]
      , negationFunction = Just negate
      , preOperators =
          [ preMul [PreOperator ((*) @Float @(Range Radians))]
          ]
      , postOperators = []
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
      , preOperators =
          [ preMul [PreOperator ((*) @Float @(Range Meters))]
          ]
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
          , "Meters" .| s2 "X" "Y" Vector2d.meters
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
          , "X Component" .| m0 Vector2d.xComponent
          , "Y Component" .| m0 Vector2d.yComponent
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
          , "X Component" .| m0 Vector2d.xComponent
          , "Y Component" .| m0 Vector2d.yComponent
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
          , "Components" .| m0 Direction2d.components
          , "X Component" .| m0 Direction2d.xComponent
          , "Y Component" .| m0 Direction2d.yComponent
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
          , "X Coordinate" .| m0 Point2d.xCoordinate
          , "Y Coordinate" .| m0 Point2d.yCoordinate
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
          , "X Coordinate" .| m0 Point2d.xCoordinate
          , "Y Coordinate" .| m0 Point2d.yCoordinate
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
          [ "T" .| s0 Curve1d.t
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
              , PreOperator ((*) @Angle @(Curve1d Unitless))
              , PreOperator ((*) @Length @(Curve1d Unitless))
              ]
          , preDiv
              [ PreOperator ((/) @Float @(Curve1d Unitless))
              , PreOperator ((/) @Angle @(Curve1d Unitless))
              , PreOperator ((/) @Length @(Curve1d Unitless))
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
              , PostOperator ((*) @(Curve1d Unitless) @Angle)
              , PostOperator ((*) @(Curve1d Unitless) @Length)
              , PostOperator ((*) @(Curve1d Unitless) @(Curve1d Unitless))
              , PostOperator ((*) @(Curve1d Unitless) @(Curve1d Radians))
              , PostOperator ((*) @(Curve1d Unitless) @(Curve1d Meters))
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
(.:) text values = (Name.parse text, values)

infixr 0 .:

(.|) :: Text -> a -> (Name, List a)
(.|) text value = (Name.parse text, [value])

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

functions :: List Function
functions = List.collect classFunctions classes

staticFunctionOverload :: FFI.Id a -> Name -> StaticFunction -> Function
staticFunctionOverload classId_ functionName staticFunction = do
  let (constraint, arguments, returnType) = StaticFunction.signature staticFunction
  Function
    { ffiName = StaticFunction.ffiName classId_ functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

staticFunctionOverloads :: FFI.Id a -> (Name, List StaticFunction) -> List Function
staticFunctionOverloads classId_ (functionName, overloads) =
  List.map (staticFunctionOverload classId_ functionName) overloads

memberFunctionOverload :: FFI.Id value -> Name -> MemberFunction value -> Function
memberFunctionOverload classId_ functionName memberFunction = do
  let (constraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  Function
    { ffiName = MemberFunction.ffiName classId_ functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments + [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

memberFunctionOverloads :: FFI.Id value -> (Name, List (MemberFunction value)) -> List Function
memberFunctionOverloads classId_ (functionName, overloads) =
  List.map (memberFunctionOverload classId_ functionName) overloads

negationOperatorInfo :: forall value. FFI value => FFI.Id value -> Maybe (value -> value) -> List Function
negationOperatorInfo classId_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = NegationOperator.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationOperator.invoke negationFunction
        }

preOperatorOverload :: FFI.Id value -> BinaryOperator.Id -> PreOperator value -> Function
preOperatorOverload classId_ operatorId operator = do
  let (lhsType, selfType, returnType) = PreOperator.signature operator
  Function
    { ffiName = PreOperator.ffiName classId_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperator.invoke operator
    }

preOperatorOverloads ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PreOperator value)) ->
  List Function
preOperatorOverloads classId_ (operatorId, overloads) =
  List.map (preOperatorOverload classId_ operatorId) overloads

postOperatorOverload :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> Function
postOperatorOverload classId_ operatorId operator = do
  let (selfType, rhsType, returnType) = PostOperator.signature operator
  Function
    { ffiName = PostOperator.ffiName classId_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperator.invoke operator
    }

postOperatorOverloads ::
  FFI.Id value ->
  (BinaryOperator.Id, List (PostOperator value)) ->
  List Function
postOperatorOverloads classId_ (operatorId, overloads) =
  List.map (postOperatorOverload classId_ operatorId) overloads

classFunctions :: Class -> List Function
classFunctions (Class classId_ staticFunctions memberFunctions maybeNegationOperator preOperators postOperators nestedClasses) =
  List.concat
    [ List.collect (staticFunctionOverloads classId_) staticFunctions
    , List.collect (memberFunctionOverloads classId_) memberFunctions
    , negationOperatorInfo classId_ maybeNegationOperator
    , List.collect (preOperatorOverloads classId_) preOperators
    , List.collect (postOperatorOverloads classId_) postOperators
    , List.collect classFunctions nestedClasses
    ]
