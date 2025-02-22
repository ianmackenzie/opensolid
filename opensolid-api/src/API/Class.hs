module API.Class
  ( Class (..)
  , Member
  , Self (Self)
  , new
  , constant
  , factory1
  , factoryU1R
  , factoryM1R
  , factory2
  , factoryU2
  , factoryU2R
  , factoryM2
  , factoryM2R
  , factory3
  , factoryU3
  , factoryM3
  , factoryM3R
  , factory4
  , factoryU4
  , factoryM4
  , factoryM4R
  , static1
  , static2
  , static3
  , staticM3
  , member0
  , memberU0
  , memberR0
  , memberM0
  , memberSM0
  , member1
  , member2
  , equality
  , comparison
  , negateSelf
  , absSelf
  , floatPlus
  , floatMinus
  , floatTimes
  , floatDivBy
  , plus
  , plusFloat
  , plusSelf
  , minus
  , minusFloat
  , minusSelf
  , times
  , timesFloat
  , timesSelf
  , divBy
  , divByFloat
  , divBySelf
  , floorDivBySelf
  , modBySelf
  , dot
  , dotSelf
  , cross
  , crossSelf
  , nested
  , functions
  )
where

import API.AbsFunction qualified as AbsFunction
import API.BinaryOperator qualified as BinaryOperator
import API.ComparisonFunction qualified as ComparisonFunction
import API.Constant (Constant (Constant))
import API.Constant qualified as Constant
import API.EqualityFunction qualified as EqualityFunction
import API.Function (Function (..))
import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
import API.NegationFunction qualified as NegationFunction
import API.PostOperator (PostOperator (PostOperator))
import API.PostOperator qualified as PostOperator
import API.PreOperator (PreOperator (PreOperator))
import API.PreOperator qualified as PreOperator
import API.StaticFunction (StaticFunction (..))
import API.StaticFunction qualified as StaticFunction
import Data.Proxy (Proxy (Proxy))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Units (Meters, Radians, SquareMeters)

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id value
    , documentation :: Text
    , constants :: List (FFI.Name, Constant)
    , staticFunctions :: List (FFI.Name, StaticFunction)
    , memberFunctions :: List (FFI.Name, MemberFunction value)
    , equalityFunction :: Maybe (value -> value -> Bool)
    , comparisonFunction :: Maybe (value -> value -> Int)
    , negationFunction :: Maybe (value -> value)
    , absFunction :: Maybe (value -> value)
    , preOperators :: List (BinaryOperator.Id, List (PreOperator value))
    , postOperators :: List (BinaryOperator.Id, List (PostOperator value))
    , nestedClasses :: List Class
    } ->
    Class

data Member value where
  Const :: FFI result => Text -> result -> Text -> Member value
  Static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
  StaticU1 :: (FFI a, FFI result) => Text -> Text -> (Tolerance Unitless => a -> result) -> Text -> Member value
  StaticM1 :: (FFI a, FFI result) => Text -> Text -> (Tolerance Meters => a -> result) -> Text -> Member value
  Static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Text -> Member value
  StaticU2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (Tolerance Unitless => a -> b -> result) -> Text -> Member value
  StaticM2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (Tolerance Meters => a -> b -> result) -> Text -> Member value
  Static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Text -> Member value
  StaticU3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (Tolerance Unitless => a -> b -> c -> result) -> Text -> Member value
  StaticM3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> result) -> Text -> Member value
  Static4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (a -> b -> c -> d -> result) -> Text -> Member value
  StaticU4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (Tolerance Unitless => a -> b -> c -> d -> result) -> Text -> Member value
  StaticM4 :: (FFI a, FFI b, FFI c, FFI d, FFI result) => Text -> Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> d -> result) -> Text -> Member value
  Member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
  MemberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Text -> Member value
  MemberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Text -> Member value
  MemberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Text -> Member value
  MemberSM0 :: (FFI value, FFI result) => Text -> (Tolerance SquareMeters => value -> result) -> Text -> Member value
  Member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Text -> Member value
  Member2 :: (FFI a, FFI b, FFI value, FFI result) => Text -> Text -> Text -> (a -> b -> value -> result) -> Text -> Member value
  Equality :: Eq value => Member value
  Comparison :: Ord value => Member value
  Negate :: Negation value => Member value
  Abs :: (value -> value) -> Member value
  PreOp :: (FFI lhs, FFI result) => BinaryOperator.Id -> (lhs -> value -> result) -> Member value
  PostOp :: (FFI rhs, FFI result) => BinaryOperator.Id -> (value -> rhs -> result) -> Member value
  Nested :: FFI nested => Text -> List (Member nested) -> Member value

new :: forall value. FFI value => Text -> List (Member value) -> Class
new classDocs members = buildClass classDocs members [] [] [] Nothing Nothing Nothing Nothing [] [] []

constant :: FFI result => Text -> result -> Text -> Member value
constant = Const

factory1 :: (FFI a, FFI value) => Text -> Text -> (a -> value) -> Text -> Member value
factory1 = Static1

factoryU1R :: (FFI a, FFI value) => Text -> Text -> (Tolerance Unitless => a -> Result x value) -> Text -> Member value
factoryU1R = StaticU1

factoryM1R :: (FFI a, FFI value) => Text -> Text -> (Tolerance Meters => a -> Result x value) -> Text -> Member value
factoryM1R = StaticM1

factory2 :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (a -> b -> value) -> Text -> Member value
factory2 = Static2

factoryU2 :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (Tolerance Unitless => a -> b -> value) -> Text -> Member value
factoryU2 = StaticU2

factoryU2R :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (Tolerance Unitless => a -> b -> Result x value) -> Text -> Member value
factoryU2R = StaticU2

factoryM2 :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (Tolerance Meters => a -> b -> value) -> Text -> Member value
factoryM2 = StaticM2

factoryM2R :: (FFI a, FFI b, FFI value) => Text -> Text -> Text -> (Tolerance Meters => a -> b -> Result x value) -> Text -> Member value
factoryM2R = StaticM2

factory3 :: (FFI a, FFI b, FFI c, FFI value) => Text -> Text -> Text -> Text -> (a -> b -> c -> value) -> Text -> Member value
factory3 = Static3

factoryU3 :: (FFI a, FFI b, FFI c, FFI value) => Text -> Text -> Text -> Text -> (Tolerance Unitless => a -> b -> c -> value) -> Text -> Member value
factoryU3 = StaticU3

factoryM3 :: (FFI a, FFI b, FFI c, FFI value) => Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> value) -> Text -> Member value
factoryM3 = StaticM3

factoryM3R :: (FFI a, FFI b, FFI c, FFI value) => Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> Result x value) -> Text -> Member value
factoryM3R = StaticM3

factory4 :: (FFI a, FFI b, FFI c, FFI d, FFI value) => Text -> Text -> Text -> Text -> Text -> (a -> b -> c -> d -> value) -> Text -> Member value
factory4 = Static4

factoryU4 :: (FFI a, FFI b, FFI c, FFI d, FFI value) => Text -> Text -> Text -> Text -> Text -> (Tolerance Unitless => a -> b -> c -> d -> value) -> Text -> Member value
factoryU4 = StaticU4

factoryM4 :: (FFI a, FFI b, FFI c, FFI d, FFI value) => Text -> Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> d -> value) -> Text -> Member value
factoryM4 = StaticM4

factoryM4R :: (FFI a, FFI b, FFI c, FFI d, FFI value) => Text -> Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> d -> Result x value) -> Text -> Member value
factoryM4R = StaticM4

static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
static1 = Static1

static2 :: (FFI a, FFI b, FFI result) => Text -> Text -> Text -> (a -> b -> result) -> Text -> Member value
static2 = Static2

static3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (a -> b -> c -> result) -> Text -> Member value
static3 = Static3

staticM3 :: (FFI a, FFI b, FFI c, FFI result) => Text -> Text -> Text -> Text -> (Tolerance Meters => a -> b -> c -> result) -> Text -> Member value
staticM3 = StaticM3

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
member0 = Member0

memberU0 :: (FFI value, FFI result) => Text -> (Tolerance Unitless => value -> result) -> Text -> Member value
memberU0 = MemberU0

memberR0 :: (FFI value, FFI result) => Text -> (Tolerance Radians => value -> result) -> Text -> Member value
memberR0 = MemberR0

memberM0 :: (FFI value, FFI result) => Text -> (Tolerance Meters => value -> result) -> Text -> Member value
memberM0 = MemberM0

memberSM0 :: (FFI value, FFI result) => Text -> (Tolerance SquareMeters => value -> result) -> Text -> Member value
memberSM0 = MemberSM0

member1 :: (FFI a, FFI value, FFI result) => Text -> Text -> (a -> value -> result) -> Text -> Member value
member1 = Member1

member2 :: (FFI a, FFI b, FFI value, FFI result) => Text -> Text -> Text -> (a -> b -> value -> result) -> Text -> Member value
member2 = Member2

data Self a = Self

equality :: Eq value => Member value
equality = Equality

comparison :: Ord value => Member value
comparison = Comparison

comparisonImpl :: Ord a => a -> a -> Int
comparisonImpl lhs rhs = case compare lhs rhs of
  LT -> -1
  EQ -> 0
  GT -> 1

negateSelf :: Negation value => Member value
negateSelf = Negate

absSelf :: (value -> value) -> Member value
absSelf = Abs

floatPlus ::
  forall value result.
  (Addition Float value result, FFI value, FFI result) =>
  Member value
floatPlus = PreOp BinaryOperator.Add ((+) :: Float -> value -> result)

floatMinus ::
  forall value result.
  (Subtraction Float value result, FFI value, FFI result) =>
  Member value
floatMinus = PreOp BinaryOperator.Sub ((-) :: Float -> value -> result)

floatTimes ::
  forall value result.
  (Multiplication Float value result, FFI value, FFI result) =>
  Member value
floatTimes = PreOp BinaryOperator.Mul ((*) :: Float -> value -> result)

floatDivBy ::
  forall value result.
  (Division Float value result, FFI value, FFI result) =>
  Member value
floatDivBy = PreOp BinaryOperator.Div ((/) :: Float -> value -> result)

plus ::
  forall rhs value result.
  (Addition value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
plus _ = PostOp BinaryOperator.Add ((+) :: value -> rhs -> result)

plusFloat ::
  forall value result.
  (Addition value Float result, FFI value, FFI result) =>
  Member value
plusFloat = plus @Float Self

plusSelf ::
  forall value result.
  (Addition value value result, FFI value, FFI result) =>
  Member value
plusSelf = plus @value Self

minus ::
  forall rhs value result.
  (Subtraction value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
minus _ = PostOp BinaryOperator.Sub ((-) :: value -> rhs -> result)

minusFloat ::
  forall value result.
  (Subtraction value Float result, FFI value, FFI result) =>
  Member value
minusFloat = minus @Float Self

minusSelf ::
  forall value result.
  (Subtraction value value result, FFI value, FFI result) =>
  Member value
minusSelf = minus @value Self

times ::
  forall rhs value result.
  (Multiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
times _ = PostOp BinaryOperator.Mul ((*) :: value -> rhs -> result)

timesFloat ::
  forall value result.
  (Multiplication value Float result, FFI value, FFI result) =>
  Member value
timesFloat = times @Float Self

timesSelf ::
  forall value result.
  (Multiplication value value result, FFI value, FFI result) =>
  Member value
timesSelf = times @value Self

divBy ::
  forall rhs value result.
  (Division value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
divBy _ = PostOp BinaryOperator.Div ((/) :: value -> rhs -> result)

divByFloat ::
  forall value result.
  (Division value Float result, FFI value, FFI result) =>
  Member value
divByFloat = divBy @Float Self

divBySelf ::
  forall value result.
  (Division value value result, FFI value, FFI result) =>
  Member value
divBySelf = divBy @value Self

floorDivBySelf :: forall value. (DivMod value, FFI value) => Member value
floorDivBySelf = PostOp BinaryOperator.FloorDiv ((//) :: value -> value -> Int)

modBySelf :: forall value. (DivMod value, FFI value) => Member value
modBySelf = PostOp BinaryOperator.Mod ((%) :: value -> value -> value)

dot ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
dot _ = PostOp BinaryOperator.Dot ((<>) :: value -> rhs -> result)

dotSelf ::
  forall value result.
  (DotMultiplication value value result, FFI value, FFI result) =>
  Member value
dotSelf = dot @value Self

cross ::
  forall rhs value result.
  (CrossMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
cross _ = PostOp BinaryOperator.Cross ((><) :: value -> rhs -> result)

crossSelf ::
  forall value result.
  (CrossMultiplication value value result, FFI value, FFI result) =>
  Member value
crossSelf = cross @value Self

nested :: FFI nested => Text -> List (Member nested) -> Member value
nested = Nested

addPreOverload ::
  BinaryOperator.Id ->
  PreOperator value ->
  List (BinaryOperator.Id, List (PreOperator value)) ->
  List (BinaryOperator.Id, List (PreOperator value))
addPreOverload operatorId overload [] = [(operatorId, [overload])]
addPreOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads + [overload]) : rest
    else first : addPreOverload operatorId overload rest

addPostOverload ::
  BinaryOperator.Id ->
  PostOperator value ->
  List (BinaryOperator.Id, List (PostOperator value)) ->
  List (BinaryOperator.Id, List (PostOperator value))
addPostOverload operatorId overload [] = [(operatorId, [overload])]
addPostOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads + [overload]) : rest
    else first : addPostOverload operatorId overload rest

buildClass ::
  forall value.
  FFI value =>
  Text ->
  List (Member value) ->
  List (FFI.Name, Constant) ->
  List (FFI.Name, StaticFunction) ->
  List (FFI.Name, MemberFunction value) ->
  Maybe (value -> value -> Bool) ->
  Maybe (value -> value -> Int) ->
  Maybe (value -> value) ->
  Maybe (value -> value) ->
  List (BinaryOperator.Id, List (PreOperator value)) ->
  List (BinaryOperator.Id, List (PostOperator value)) ->
  List Class ->
  Class
buildClass
  classDocs
  members
  constantsAcc
  staticFunctionsAcc
  memberFunctionsAcc
  equalityFunctionAcc
  comparisonFunctionAcc
  negationFunctionAcc
  absFunctionAcc
  preOperatorsAcc
  postOperatorsAcc
  nestedClassesAcc =
    case members of
      [] ->
        Class
          { id = case FFI.typeOf @value Proxy of
              FFI.Class (FFI.Id Proxy names) -> FFI.Id Proxy names
              _ -> internalError "Every class defined in the API must correspond to an FFI type with class representation"
          , documentation = classDocs
          , constants = constantsAcc
          , staticFunctions = staticFunctionsAcc
          , memberFunctions = memberFunctionsAcc
          , equalityFunction = equalityFunctionAcc
          , comparisonFunction = comparisonFunctionAcc
          , negationFunction = negationFunctionAcc
          , absFunction = absFunctionAcc
          , preOperators = preOperatorsAcc
          , postOperators = postOperatorsAcc
          , nestedClasses = nestedClassesAcc
          }
      first : rest -> do
        let addStatic name staticFunction =
              buildClass
                classDocs
                rest
                constantsAcc
                (staticFunctionsAcc + [(FFI.name name, staticFunction)])
                memberFunctionsAcc
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                absFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        let addMember name memberFunction =
              buildClass
                classDocs
                rest
                constantsAcc
                staticFunctionsAcc
                (memberFunctionsAcc + [(FFI.name name, memberFunction)])
                equalityFunctionAcc
                comparisonFunctionAcc
                negationFunctionAcc
                absFunctionAcc
                preOperatorsAcc
                postOperatorsAcc
                nestedClassesAcc
        case first of
          Const name value documentation ->
            buildClass
              classDocs
              rest
              (constantsAcc + [(FFI.name name, Constant value documentation)])
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Static1 name arg1 f staticDocs ->
            addStatic name (StaticFunction1 (FFI.name arg1) f staticDocs)
          StaticU1 name arg1 f staticDocs ->
            addStatic name (StaticFunctionU1 (FFI.name arg1) f staticDocs)
          StaticM1 name arg1 f staticDocs ->
            addStatic name (StaticFunctionM1 (FFI.name arg1) f staticDocs)
          Static2 name arg1 arg2 f staticDocs ->
            addStatic name (StaticFunction2 (FFI.name arg1) (FFI.name arg2) f staticDocs)
          StaticU2 name arg1 arg2 f staticDocs ->
            addStatic name (StaticFunctionU2 (FFI.name arg1) (FFI.name arg2) f staticDocs)
          StaticM2 name arg1 arg2 f staticDocs ->
            addStatic name (StaticFunctionM2 (FFI.name arg1) (FFI.name arg2) f staticDocs)
          Static3 name arg1 arg2 arg3 f staticDocs ->
            addStatic name (StaticFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f staticDocs)
          StaticU3 name arg1 arg2 arg3 f staticDocs ->
            addStatic name (StaticFunctionU3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f staticDocs)
          StaticM3 name arg1 arg2 arg3 f staticDocs ->
            addStatic name (StaticFunctionM3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f staticDocs)
          Static4 name arg1 arg2 arg3 arg4 f staticDocs ->
            addStatic name (StaticFunction4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f staticDocs)
          StaticU4 name arg1 arg2 arg3 arg4 f staticDocs ->
            addStatic name (StaticFunctionU4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f staticDocs)
          StaticM4 name arg1 arg2 arg3 arg4 f staticDocs ->
            addStatic name (StaticFunctionM4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f staticDocs)
          Member0 name f memberDocs ->
            addMember name (MemberFunction0 f memberDocs)
          MemberU0 name f memberDocs ->
            addMember name (MemberFunctionU0 f memberDocs)
          MemberR0 name f memberDocs ->
            addMember name (MemberFunctionR0 f memberDocs)
          MemberM0 name f memberDocs ->
            addMember name (MemberFunctionM0 f memberDocs)
          MemberSM0 name f memberDocs ->
            addMember name (MemberFunctionSM0 f memberDocs)
          Member1 name arg1 f memberDocs ->
            addMember name (MemberFunction1 (FFI.name arg1) f memberDocs)
          Member2 name arg1 arg2 f memberDocs ->
            addMember name (MemberFunction2 (FFI.name arg1) (FFI.name arg2) f memberDocs)
          Equality ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              (Just (==))
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Comparison ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              (Just comparisonImpl)
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Negate ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              (Just negate)
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          Abs function ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              (Just function)
              preOperatorsAcc
              postOperatorsAcc
              nestedClassesAcc
          PreOp operatorId operator ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              (addPreOverload operatorId (PreOperator operator) preOperatorsAcc)
              postOperatorsAcc
              nestedClassesAcc
          PostOp operatorId operator ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              (addPostOverload operatorId (PostOperator operator) postOperatorsAcc)
              nestedClassesAcc
          Nested nestedDocstring nestedMembers ->
            buildClass
              classDocs
              rest
              constantsAcc
              staticFunctionsAcc
              memberFunctionsAcc
              equalityFunctionAcc
              comparisonFunctionAcc
              negationFunctionAcc
              absFunctionAcc
              preOperatorsAcc
              postOperatorsAcc
              (nestedClassesAcc + [new nestedDocstring nestedMembers])

----- FUNCTION COLLECTION -----

functions :: Class -> List Function
functions
  ( Class
      classId_
      _
      constants
      staticFunctions
      memberFunctions
      maybeEqualityFunction
      maybeComparisonFunction
      maybeNegationFunction
      maybeAbsFunction
      preOperators
      postOperators
      nestedClasses
    ) =
    List.concat
      [ List.map (constantFunctionInfo classId_) constants
      , List.map (staticFunctionInfo classId_) staticFunctions
      , List.map (memberFunctionInfo classId_) memberFunctions
      , equalityFunctionInfo classId_ maybeEqualityFunction
      , comparisonFunctionInfo classId_ maybeComparisonFunction
      , negationFunctionInfo classId_ maybeNegationFunction
      , absFunctionInfo classId_ maybeAbsFunction
      , List.collect (preOperatorOverloads classId_) preOperators
      , List.collect (postOperatorOverloads classId_) postOperators
      , List.collect functions nestedClasses
      ]

constantFunctionInfo :: FFI.Id a -> (FFI.Name, Constant) -> Function
constantFunctionInfo classId_ (constantName, const@(Constant value _)) =
  Function
    { ffiName = Constant.ffiName classId_ constantName
    , constraint = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
    , invoke = Constant.invoke const
    }

staticFunctionInfo :: FFI.Id a -> (FFI.Name, StaticFunction) -> Function
staticFunctionInfo classId_ (functionName, staticFunction) = do
  let (constraint, arguments, returnType) = StaticFunction.signature staticFunction
  Function
    { ffiName = StaticFunction.ffiName classId_ functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

memberFunctionInfo :: FFI.Id value -> (FFI.Name, MemberFunction value) -> Function
memberFunctionInfo classId_ (functionName, memberFunction) = do
  let (constraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  Function
    { ffiName = MemberFunction.ffiName classId_ functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments + [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

negationFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value) ->
  List Function
negationFunctionInfo classId_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = NegationFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationFunction.invoke negationFunction
        }

absFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value) ->
  List Function
absFunctionInfo classId_ maybeAbsFunction = case maybeAbsFunction of
  Nothing -> []
  Just absFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = AbsFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = AbsFunction.invoke absFunction
        }

equalityFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value -> Bool) ->
  List Function
equalityFunctionInfo classId_ maybeEqualityFunction = case maybeEqualityFunction of
  Nothing -> []
  Just equalityFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = EqualityFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Bool Proxy
        , invoke = EqualityFunction.invoke equalityFunction
        }

comparisonFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Id value ->
  Maybe (value -> value -> Int) ->
  List Function
comparisonFunctionInfo classId_ maybeComparisonFunction = case maybeComparisonFunction of
  Nothing -> []
  Just comparisonFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = ComparisonFunction.ffiName classId_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Int Proxy
        , invoke = ComparisonFunction.invoke comparisonFunction
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
