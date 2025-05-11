module API.Class
  ( Class (..)
  , Member
  , Self (Self)
  , new
  , static
  , upcast
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
  , memberM3
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

import API.AbsFunction (AbsFunction (AbsFunction))
import API.AbsFunction qualified as AbsFunction
import API.BinaryOperator qualified as BinaryOperator
import API.ComparisonFunction (ComparisonFunction (ComparisonFunction))
import API.ComparisonFunction qualified as ComparisonFunction
import API.Constant (Constant (Constant))
import API.Constant qualified as Constant
import API.Constructor (Constructor (..))
import API.Constructor qualified as Constructor
import API.EqualityFunction (EqualityFunction (EqualityFunction))
import API.EqualityFunction qualified as EqualityFunction
import API.Function (Function (..))
import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
import API.NegationFunction (NegationFunction (NegationFunction))
import API.NegationFunction qualified as NegationFunction
import API.PostOperator (PostOperator (PostOperator))
import API.PostOperator qualified as PostOperator
import API.PreOperator (PreOperator (PreOperator))
import API.PreOperator qualified as PreOperator
import API.StaticFunction (StaticFunction (..))
import API.StaticFunction qualified as StaticFunction
import API.Upcast (Upcast (Upcast))
import API.Upcast qualified as Upcast
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Units (Meters, Radians, SquareMeters)

data Class where
  Class ::
    { name :: FFI.ClassName
    , documentation :: Text
    , toParent :: Maybe Upcast
    , constants :: List (FFI.Name, Constant)
    , constructor :: Maybe Constructor
    , staticFunctions :: List (FFI.Name, StaticFunction)
    , memberFunctions :: List (FFI.Name, MemberFunction)
    , equalityFunction :: Maybe EqualityFunction
    , comparisonFunction :: Maybe ComparisonFunction
    , negationFunction :: Maybe NegationFunction
    , absFunction :: Maybe AbsFunction
    , preOperators :: List (BinaryOperator.Id, List PreOperator)
    , postOperators :: List (BinaryOperator.Id, List PostOperator)
    , nestedClasses :: List Class
    } ->
    Class

data Member value where
  ToParent :: Upcast -> Member value
  Const :: FFI.Name -> Constant -> Member value
  Constructor :: Constructor -> Member value
  Static :: FFI.Name -> StaticFunction -> Member value
  Member :: FFI.Name -> MemberFunction -> Member value
  Equality :: EqualityFunction -> Member value
  Comparison :: ComparisonFunction -> Member value
  Negate :: NegationFunction -> Member value
  Abs :: AbsFunction -> Member value
  PreOp :: (FFI value, FFI lhs, FFI result) => BinaryOperator.Id -> (lhs -> value -> result) -> Member value
  PostOp :: (FFI value, FFI rhs, FFI result) => BinaryOperator.Id -> (value -> rhs -> result) -> Member value
  Nested :: FFI nested => Text -> List (Member nested) -> Member value

new :: forall value. FFI value => Text -> List (Member value) -> Class
new givenDocumentation members =
  buildClass members (init (FFI.className @value Proxy) givenDocumentation)

static :: Text -> Text -> List (Member Void) -> Class
static className givenDocumentation members =
  buildClass members (init (FFI.staticClassName className) givenDocumentation)

constant :: FFI result => Text -> result -> Text -> Member value
constant name value docs = Const (FFI.name name) (Constant value docs)

upcast :: (FFI parent, FFI value) => (value -> parent) -> Member value
upcast = ToParent . Upcast

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

memberM3 ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  Text ->
  Member value
memberM3 name arg1 arg2 arg3 f docs =
  Member (FFI.name name) (MemberFunctionM3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

data Self a = Self

equality :: forall value. (FFI value, Eq value) => Member value
equality = Equality (EqualityFunction ((==) @value))

comparison :: forall value. (FFI value, Ord value) => Member value
comparison = Comparison (ComparisonFunction (comparisonImpl @value))

comparisonImpl :: Ord a => a -> a -> Int
comparisonImpl lhs rhs = case compare lhs rhs of LT -> -1; EQ -> 0; GT -> 1

negateSelf :: forall value. (FFI value, Negation value) => Member value
negateSelf = Negate (NegationFunction (negate @value))

absSelf :: FFI value => (value -> value) -> Member value
absSelf = Abs . AbsFunction

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

init :: FFI.ClassName -> Text -> Class
init givenName givenDocumentation =
  Class
    { name = givenName
    , documentation = givenDocumentation
    , toParent = Nothing
    , constants = []
    , constructor = Nothing
    , staticFunctions = []
    , memberFunctions = []
    , equalityFunction = Nothing
    , comparisonFunction = Nothing
    , negationFunction = Nothing
    , absFunction = Nothing
    , preOperators = []
    , postOperators = []
    , nestedClasses = []
    }

buildClass :: List (Member value) -> Class -> Class
buildClass members built = case members of
  [] -> built
  first : rest -> buildClass rest $ case first of
    ToParent toParent ->
      built{toParent = Just toParent}
    Const name const ->
      built{constants = constants built <> [(name, const)]}
    Constructor constructor ->
      built{constructor = Just constructor}
    Static name staticFunction ->
      built{staticFunctions = staticFunctions built <> [(name, staticFunction)]}
    Member name memberFunction ->
      built{memberFunctions = memberFunctions built <> [(name, memberFunction)]}
    Equality equalityFunction ->
      built{equalityFunction = Just equalityFunction}
    Comparison comparisonFunction ->
      built{comparisonFunction = Just comparisonFunction}
    Negate negationFunction ->
      built{negationFunction = Just negationFunction}
    Abs absFunction ->
      built{absFunction = Just absFunction}
    PreOp operatorId operator ->
      built{preOperators = addPreOverload operatorId (PreOperator operator) (preOperators built)}
    PostOp operatorId operator ->
      built{postOperators = addPostOverload operatorId (PostOperator operator) (postOperators built)}
    Nested nestedDocstring nestedMembers ->
      built{nestedClasses = nestedClasses built <> [new nestedDocstring nestedMembers]}

----- FUNCTION COLLECTION -----

functions :: Class -> List Function
functions
  ( Class
      className
      _
      toParent
      constants
      constructor
      staticFunctions
      memberFunctions
      equalityFunction
      comparisonFunction
      negationFunction
      absFunction
      preOperators
      postOperators
      nestedClasses
    ) =
    List.concat
      [ upcastInfo className toParent
      , List.map (constantFunctionInfo className) constants
      , constructorInfo className constructor
      , List.map (staticFunctionInfo className) staticFunctions
      , List.map (memberFunctionInfo className) memberFunctions
      , equalityFunctionInfo className equalityFunction
      , comparisonFunctionInfo className comparisonFunction
      , negationFunctionInfo className negationFunction
      , absFunctionInfo className absFunction
      , List.collect (preOperatorOverloads className) preOperators
      , List.collect (postOperatorOverloads className) postOperators
      , List.collect functions nestedClasses
      ]

upcastInfo :: FFI.ClassName -> Maybe Upcast -> List Function
upcastInfo className maybeToParent = case maybeToParent of
  Nothing -> []
  Just toParent -> do
    List.singleton $
      Function
        { ffiName = Upcast.ffiName className
        , constraint = Nothing
        , argumentTypes = [FFI.Class className]
        , returnType = FFI.Class (Upcast.parentClassName toParent)
        , invoke = Upcast.invoke toParent
        }

constantFunctionInfo :: FFI.ClassName -> (FFI.Name, Constant) -> Function
constantFunctionInfo className (constantName, const@(Constant value _)) =
  Function
    { ffiName = Constant.ffiName className constantName
    , constraint = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
    , invoke = Constant.invoke const
    }

constructorInfo :: FFI.ClassName -> Maybe Constructor -> List Function
constructorInfo className maybeConstructor = case maybeConstructor of
  Nothing -> []
  Just constructor -> do
    let arguments = Constructor.signature constructor
    List.singleton $
      Function
        { ffiName = Constructor.ffiName className constructor
        , constraint = Nothing
        , argumentTypes = List.map Pair.second arguments
        , returnType = FFI.Class className
        , invoke = Constructor.invoke constructor
        }

staticFunctionInfo :: FFI.ClassName -> (FFI.Name, StaticFunction) -> Function
staticFunctionInfo className (functionName, staticFunction) = do
  let (constraint, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = StaticFunction.ffiName className functionName staticFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

memberFunctionInfo :: FFI.ClassName -> (FFI.Name, MemberFunction) -> Function
memberFunctionInfo className (functionName, memberFunction) = do
  let selfType = FFI.Class className
  let (constraint, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = MemberFunction.ffiName className functionName memberFunction
    , constraint
    , argumentTypes = List.map Pair.second arguments <> [selfType]
    , returnType
    , invoke = MemberFunction.invoke memberFunction
    }

negationFunctionInfo :: FFI.ClassName -> Maybe NegationFunction -> List Function
negationFunctionInfo className maybeNegationFunction = case maybeNegationFunction of
  Nothing -> []
  Just negationFunction -> do
    let selfType = FFI.Class className
    List.singleton $
      Function
        { ffiName = NegationFunction.ffiName className
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = NegationFunction.invoke negationFunction
        }

absFunctionInfo :: FFI.ClassName -> Maybe AbsFunction -> List Function
absFunctionInfo className maybeAbsFunction = case maybeAbsFunction of
  Nothing -> []
  Just absFunction -> do
    let selfType = FFI.Class className
    List.singleton $
      Function
        { ffiName = AbsFunction.ffiName className
        , constraint = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = AbsFunction.invoke absFunction
        }

equalityFunctionInfo :: FFI.ClassName -> Maybe EqualityFunction -> List Function
equalityFunctionInfo className maybeEqualityFunction = case maybeEqualityFunction of
  Nothing -> []
  Just equalityFunction -> do
    let selfType = FFI.Class className
    List.singleton $
      Function
        { ffiName = EqualityFunction.ffiName className
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Bool Proxy
        , invoke = EqualityFunction.invoke equalityFunction
        }

comparisonFunctionInfo :: FFI.ClassName -> Maybe ComparisonFunction -> List Function
comparisonFunctionInfo className maybeComparisonFunction = case maybeComparisonFunction of
  Nothing -> []
  Just comparisonFunction -> do
    let selfType = FFI.Class className
    List.singleton $
      Function
        { ffiName = ComparisonFunction.ffiName className
        , constraint = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Int Proxy
        , invoke = ComparisonFunction.invoke comparisonFunction
        }

preOperatorOverload :: FFI.ClassName -> BinaryOperator.Id -> PreOperator -> Function
preOperatorOverload className operatorId operator = do
  let selfType = FFI.Class className
  let (lhsType, returnType) = PreOperator.signature operator
  Function
    { ffiName = PreOperator.ffiName className operatorId operator
    , constraint = Nothing
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperator.invoke operator
    }

preOperatorOverloads :: FFI.ClassName -> (BinaryOperator.Id, List PreOperator) -> List Function
preOperatorOverloads className (operatorId, overloads) =
  List.map (preOperatorOverload className operatorId) overloads

postOperatorOverload :: FFI.ClassName -> BinaryOperator.Id -> PostOperator -> Function
postOperatorOverload className operatorId operator = do
  let selfType = FFI.Class className
  let (rhsType, returnType) = PostOperator.signature operator
  Function
    { ffiName = PostOperator.ffiName className operatorId operator
    , constraint = Nothing
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperator.invoke operator
    }

postOperatorOverloads :: FFI.ClassName -> (BinaryOperator.Id, List PostOperator) -> List Function
postOperatorOverloads className (operatorId, overloads) =
  List.map (postOperatorOverload className operatorId) overloads
