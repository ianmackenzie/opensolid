module API.Class
  ( Class (..)
  , Member
  , Self (Self)
  , new
  , constant
  , constructor1
  , constructor2
  , constructor3
  , constructor4
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
  , memberS0
  , member1
  , member2
  , memberU2
  , memberM2
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
  , dotProduct
  , dotSelf
  , crossProduct
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
import API.Constructor (Constructor (..))
import API.Constructor qualified as Constructor
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
    { ffiClass :: FFI.Class
    , documentation :: Text
    , constants :: List (FFI.Name, Constant)
    , constructor :: Maybe (Constructor value)
    , staticFunctions :: List (FFI.Name, StaticFunction)
    , memberFunctions :: List (FFI.Name, MemberFunction)
    , equalityFunction :: Maybe (value -> value -> Bool)
    , comparisonFunction :: Maybe (value -> value -> Int)
    , negationFunction :: Maybe (value -> value)
    , absFunction :: Maybe (value -> value)
    , preOperators :: List (BinaryOperator.Id, List PreOperator)
    , postOperators :: List (BinaryOperator.Id, List PostOperator)
    , nestedClasses :: List Class
    } ->
    Class

data Member value where
  Const :: FFI result => Text -> result -> Text -> Member value
  Constructor :: Constructor value -> Member value
  Static :: FFI.Name -> StaticFunction -> Member value
  Member :: FFI.Name -> MemberFunction -> Member value
  Equality :: Eq value => Member value
  Comparison :: Ord value => Member value
  Negate :: Negation value => Member value
  Abs :: (value -> value) -> Member value
  PreOp :: (FFI lhs, FFI result) => BinaryOperator.Id -> (lhs -> value -> result) -> Member value
  PostOp :: (FFI rhs, FFI result) => BinaryOperator.Id -> (value -> rhs -> result) -> Member value
  Nested :: FFI nested => Text -> List (Member nested) -> Member value

new :: forall value. FFI value => Text -> List (Member value) -> Class
new classDocs members =
  buildClass classDocs members [] Nothing [] [] Nothing Nothing Nothing Nothing [] [] []

constant :: FFI result => Text -> result -> Text -> Member value
constant = Const

constructor1 :: (FFI a, FFI value) => Text -> (a -> value) -> Text -> Member value
constructor1 arg1 f docs = Constructor (Constructor1 (FFI.name arg1) f docs)

constructor2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  (a -> b -> value) ->
  Text ->
  Member value
constructor2 arg1 arg2 f docs = Constructor (Constructor2 (FFI.name arg1) (FFI.name arg2) f docs)

constructor3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value) ->
  Text ->
  Member value
constructor3 arg1 arg2 arg3 f docs =
  Constructor (Constructor3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

constructor4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> value) ->
  Text ->
  Member value
constructor4 arg1 arg2 arg3 arg4 f docs =
  Constructor (Constructor4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs)

factory1 :: (FFI a, FFI value) => Text -> Text -> (a -> value) -> Text -> Member value
factory1 = static1

factoryU1R ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (Tolerance Unitless => a -> Result x value) ->
  Text ->
  Member value
factoryU1R = staticU1

factoryM1R ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> Result x value) ->
  Text ->
  Member value
factoryM1R = staticM1

factory2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> value) ->
  Text ->
  Member value
factory2 = static2

factoryU2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> value) ->
  Text ->
  Member value
factoryU2 = staticU2

factoryU2R ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> Result x value) ->
  Text ->
  Member value
factoryU2R = staticU2

factoryM2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value) ->
  Text ->
  Member value
factoryM2 = staticM2

factoryM2R ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> Result x value) ->
  Text ->
  Member value
factoryM2R = staticM2

factory3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value) ->
  Text ->
  Member value
factory3 = static3

factoryU3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> c -> value) ->
  Text ->
  Member value
factoryU3 = staticU3

factoryM3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> value) ->
  Text ->
  Member value
factoryM3 = staticM3

factoryM3R ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> Result x value) ->
  Text ->
  Member value
factoryM3R = staticM3

factory4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> value) ->
  Text ->
  Member value
factory4 = static4

factoryU4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> c -> d -> value) ->
  Text ->
  Member value
factoryU4 = staticU4

factoryM4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> value) ->
  Text ->
  Member value
factoryM4 = staticM4

factoryM4R ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> Result x value) ->
  Text ->
  Member value
factoryM4R = staticM4

static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
static1 name arg1 f docs = Static (FFI.name name) (StaticFunction1 (FFI.name arg1) f docs)

staticU1 ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Unitless => a -> result) ->
  Text ->
  Member value
staticU1 name arg1 f docs = Static (FFI.name name) (StaticFunctionU1 (FFI.name arg1) f docs)

staticM1 ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> result) ->
  Text ->
  Member value
staticM1 name arg1 f docs = Static (FFI.name name) (StaticFunctionM1 (FFI.name arg1) f docs)

static2 ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> result) ->
  Text ->
  Member value
static2 name arg1 arg2 f docs =
  Static (FFI.name name) (StaticFunction2 (FFI.name arg1) (FFI.name arg2) f docs)

staticU2 ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> result) ->
  Text ->
  Member value
staticU2 name arg1 arg2 f docs =
  Static (FFI.name name) (StaticFunctionU2 (FFI.name arg1) (FFI.name arg2) f docs)

staticM2 ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> result) ->
  Text ->
  Member value
staticM2 name arg1 arg2 f docs =
  Static (FFI.name name) (StaticFunctionM2 (FFI.name arg1) (FFI.name arg2) f docs)

static3 ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> result) ->
  Text ->
  Member value
static3 name arg1 arg2 arg3 f docs =
  Static (FFI.name name) (StaticFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

staticU3 ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> c -> result) ->
  Text ->
  Member value
staticU3 name arg1 arg2 arg3 f docs =
  Static (FFI.name name) (StaticFunctionU3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

staticM3 ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> result) ->
  Text ->
  Member value
staticM3 name arg1 arg2 arg3 f docs =
  Static (FFI.name name) (StaticFunctionM3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

static4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> result) ->
  Text ->
  Member value
static4 name arg1 arg2 arg3 arg4 f docs =
  Static (FFI.name name) $
    StaticFunction4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs

staticU4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> c -> d -> result) ->
  Text ->
  Member value
staticU4 name arg1 arg2 arg3 arg4 f docs =
  Static (FFI.name name) $
    StaticFunctionU4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs

staticM4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> result) ->
  Text ->
  Member value
staticM4 name arg1 arg2 arg3 arg4 f docs =
  Static (FFI.name name) $
    StaticFunctionM4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
member0 name f docs = Member (FFI.name name) (MemberFunction0 f docs)

memberU0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Unitless => value -> result) ->
  Text ->
  Member value
memberU0 name f docs = Member (FFI.name name) (MemberFunctionU0 f docs)

memberR0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Radians => value -> result) ->
  Text ->
  Member value
memberR0 name f docs = Member (FFI.name name) (MemberFunctionR0 f docs)

memberM0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> result) ->
  Text ->
  Member value
memberM0 name f docs = Member (FFI.name name) (MemberFunctionM0 f docs)

memberS0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance SquareMeters => value -> result) ->
  Text ->
  Member value
memberS0 name f docs = Member (FFI.name name) (MemberFunctionS0 f docs)

member1 ::
  (FFI a, FFI value, FFI result) =>
  Text ->
  Text ->
  (a -> value -> result) ->
  Text ->
  Member value
member1 name arg1 f docs = Member (FFI.name name) (MemberFunction1 (FFI.name arg1) f docs)

member2 ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> value -> result) ->
  Text ->
  Member value
member2 name arg1 arg2 f docs =
  Member (FFI.name name) (MemberFunction2 (FFI.name arg1) (FFI.name arg2) f docs)

memberU2 ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Unitless => a -> b -> value -> result) ->
  Text ->
  Member value
memberU2 name arg1 arg2 f docs =
  Member (FFI.name name) (MemberFunctionU2 (FFI.name arg1) (FFI.name arg2) f docs)

memberM2 ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value -> result) ->
  Text ->
  Member value
memberM2 name arg1 arg2 f docs =
  Member (FFI.name name) (MemberFunctionM2 (FFI.name arg1) (FFI.name arg2) f docs)

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

dotProduct ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
dotProduct _ = PostOp BinaryOperator.Dot (dot :: value -> rhs -> result)

dotSelf ::
  forall value result.
  (DotMultiplication value value result, FFI value, FFI result) =>
  Member value
dotSelf = dotProduct @value Self

crossProduct ::
  forall rhs value result.
  (CrossMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
crossProduct _ = PostOp BinaryOperator.Cross (cross :: value -> rhs -> result)

crossSelf ::
  forall value result.
  (CrossMultiplication value value result, FFI value, FFI result) =>
  Member value
crossSelf = crossProduct @value Self

nested :: FFI nested => Text -> List (Member nested) -> Member value
nested = Nested

addPreOverload ::
  BinaryOperator.Id ->
  PreOperator ->
  List (BinaryOperator.Id, List PreOperator) ->
  List (BinaryOperator.Id, List PreOperator)
addPreOverload operatorId overload [] = [(operatorId, [overload])]
addPreOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads <> [overload]) : rest
    else first : addPreOverload operatorId overload rest

addPostOverload ::
  BinaryOperator.Id ->
  PostOperator ->
  List (BinaryOperator.Id, List PostOperator) ->
  List (BinaryOperator.Id, List PostOperator)
addPostOverload operatorId overload [] = [(operatorId, [overload])]
addPostOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads <> [overload]) : rest
    else first : addPostOverload operatorId overload rest

buildClass ::
  forall value.
  FFI value =>
  Text ->
  List (Member value) ->
  List (FFI.Name, Constant) ->
  Maybe (Constructor value) ->
  List (FFI.Name, StaticFunction) ->
  List (FFI.Name, MemberFunction) ->
  Maybe (value -> value -> Bool) ->
  Maybe (value -> value -> Int) ->
  Maybe (value -> value) ->
  Maybe (value -> value) ->
  List (BinaryOperator.Id, List PreOperator) ->
  List (BinaryOperator.Id, List PostOperator) ->
  List Class ->
  Class
buildClass
  classDocs
  members
  constantsAcc
  ctorAcc
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
          { ffiClass = case FFI.typeOf @value Proxy of
              FFI.Class ffiClass -> ffiClass
              _ -> internalError "Every class defined in the API must correspond to an FFI type with class representation"
          , documentation = classDocs
          , constants = constantsAcc
          , constructor = ctorAcc
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
      first : rest -> case first of
        Const name value documentation ->
          buildClass
            classDocs
            rest
            (constantsAcc <> [(FFI.name name, Constant value documentation)])
            ctorAcc
            staticFunctionsAcc
            memberFunctionsAcc
            equalityFunctionAcc
            comparisonFunctionAcc
            negationFunctionAcc
            absFunctionAcc
            preOperatorsAcc
            postOperatorsAcc
            nestedClassesAcc
        Constructor constructor ->
          buildClass
            classDocs
            rest
            constantsAcc
            (Just constructor)
            staticFunctionsAcc
            memberFunctionsAcc
            equalityFunctionAcc
            comparisonFunctionAcc
            negationFunctionAcc
            absFunctionAcc
            preOperatorsAcc
            postOperatorsAcc
            nestedClassesAcc
        Static name staticFunction ->
          buildClass
            classDocs
            rest
            constantsAcc
            ctorAcc
            (staticFunctionsAcc <> [(name, staticFunction)])
            memberFunctionsAcc
            equalityFunctionAcc
            comparisonFunctionAcc
            negationFunctionAcc
            absFunctionAcc
            preOperatorsAcc
            postOperatorsAcc
            nestedClassesAcc
        Member name memberFunction ->
          buildClass
            classDocs
            rest
            constantsAcc
            ctorAcc
            staticFunctionsAcc
            (memberFunctionsAcc <> [(name, memberFunction)])
            equalityFunctionAcc
            comparisonFunctionAcc
            negationFunctionAcc
            absFunctionAcc
            preOperatorsAcc
            postOperatorsAcc
            nestedClassesAcc
        Equality ->
          buildClass
            classDocs
            rest
            constantsAcc
            ctorAcc
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
            ctorAcc
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
            ctorAcc
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
            ctorAcc
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
            ctorAcc
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
            ctorAcc
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
            ctorAcc
            staticFunctionsAcc
            memberFunctionsAcc
            equalityFunctionAcc
            comparisonFunctionAcc
            negationFunctionAcc
            absFunctionAcc
            preOperatorsAcc
            postOperatorsAcc
            (nestedClassesAcc <> [new nestedDocstring nestedMembers])

----- FUNCTION COLLECTION -----

functions :: Class -> List Function
functions
  ( Class
      ffiClass_
      _
      constants
      maybeConstructor
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
      [ List.map (constantFunctionInfo ffiClass_) constants
      , constructorInfo ffiClass_ maybeConstructor
      , List.map (staticFunctionInfo ffiClass_) staticFunctions
      , List.map (memberFunctionInfo ffiClass_) memberFunctions
      , equalityFunctionInfo ffiClass_ maybeEqualityFunction
      , comparisonFunctionInfo ffiClass_ maybeComparisonFunction
      , negationFunctionInfo ffiClass_ maybeNegationFunction
      , absFunctionInfo ffiClass_ maybeAbsFunction
      , List.collect (preOperatorOverloads ffiClass_) preOperators
      , List.collect (postOperatorOverloads ffiClass_) postOperators
      , List.collect functions nestedClasses
      ]

constantFunctionInfo :: FFI.Class -> (FFI.Name, Constant) -> Function
constantFunctionInfo ffiClass_ (constantName, const@(Constant value _)) =
  Function
    { ffiName = Constant.ffiName ffiClass_ constantName
    , constraint = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
    , invoke = Constant.invoke const
    }

constructorInfo ::
  forall value.
  FFI value =>
  FFI.Class ->
  Maybe (Constructor value) ->
  List Function
constructorInfo ffiClass_ maybeConstructor = case maybeConstructor of
  Nothing -> []
  Just constructor -> do
    let (arguments, _) = Constructor.signature constructor
    List.singleton $
      Function
        { ffiName = Constructor.ffiName ffiClass_ constructor
        , constraint = Nothing
        , argumentTypes = List.map Pair.second arguments
        , returnType = FFI.typeOf @value Proxy
        , invoke = Constructor.invoke constructor
        }

staticFunctionInfo :: FFI.Class -> (FFI.Name, StaticFunction) -> Function
staticFunctionInfo ffiClass_ (functionName, staticFunction) = do
  let (constraint, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = StaticFunction.ffiName ffiClass_ functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

memberFunctionInfo :: FFI.Class -> (FFI.Name, MemberFunction) -> Function
memberFunctionInfo ffiClass_ (functionName, memberFunction) = do
  let selfType = FFI.Class ffiClass_
  let (constraint, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = MemberFunction.ffiName ffiClass_ functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments <> [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

negationFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Class ->
  Maybe (value -> value) ->
  List Function
negationFunctionInfo ffiClass_ maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = NegationFunction.ffiName ffiClass_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationFunction.invoke negationFunction
        }

absFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Class ->
  Maybe (value -> value) ->
  List Function
absFunctionInfo ffiClass_ maybeAbsFunction = case maybeAbsFunction of
  Nothing -> []
  Just absFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = AbsFunction.ffiName ffiClass_
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = AbsFunction.invoke absFunction
        }

equalityFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Class ->
  Maybe (value -> value -> Bool) ->
  List Function
equalityFunctionInfo ffiClass_ maybeEqualityFunction = case maybeEqualityFunction of
  Nothing -> []
  Just equalityFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = EqualityFunction.ffiName ffiClass_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Bool Proxy
        , invoke = EqualityFunction.invoke equalityFunction
        }

comparisonFunctionInfo ::
  forall value.
  FFI value =>
  FFI.Class ->
  Maybe (value -> value -> Int) ->
  List Function
comparisonFunctionInfo ffiClass_ maybeComparisonFunction = case maybeComparisonFunction of
  Nothing -> []
  Just comparisonFunction -> do
    let selfType = FFI.typeOf @value Proxy
    List.singleton $
      Function
        { ffiName = ComparisonFunction.ffiName ffiClass_
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Int Proxy
        , invoke = ComparisonFunction.invoke comparisonFunction
        }

preOperatorOverload :: FFI.Class -> BinaryOperator.Id -> PreOperator -> Function
preOperatorOverload ffiClass_ operatorId operator = do
  let selfType = FFI.Class ffiClass_
  let (lhsType, returnType) = PreOperator.signature operator
  Function
    { ffiName = PreOperator.ffiName ffiClass_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperator.invoke operator
    }

preOperatorOverloads :: FFI.Class -> (BinaryOperator.Id, List PreOperator) -> List Function
preOperatorOverloads ffiClass_ (operatorId, overloads) =
  List.map (preOperatorOverload ffiClass_ operatorId) overloads

postOperatorOverload :: FFI.Class -> BinaryOperator.Id -> PostOperator -> Function
postOperatorOverload ffiClass_ operatorId operator = do
  let selfType = FFI.Class ffiClass_
  let (rhsType, returnType) = PostOperator.signature operator
  Function
    { ffiName = PostOperator.ffiName ffiClass_ operatorId operator
    , constraint = Nothing
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperator.invoke operator
    }

postOperatorOverloads :: FFI.Class -> (BinaryOperator.Id, List PostOperator) -> List Function
postOperatorOverloads ffiClass_ (operatorId, overloads) =
  List.map (postOperatorOverload ffiClass_ operatorId) overloads
