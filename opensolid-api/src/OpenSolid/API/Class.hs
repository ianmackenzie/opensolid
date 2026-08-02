-- Needed for 'property', since call sites will explicitly specify the property name
{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.API.Class
  ( Class (..)
  , Member
  , new
  , static
  , constant
  , constructor1
  , constructor2
  , constructor3
  , constructor4
  , factory1
  , factoryM1
  , factoryU1R
  , factoryM1R
  , factory2
  , factory2R
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
  , factory5
  , factory6
  , static1
  , static2
  , static3
  , staticM3
  , static10
  , property
  , member0
  , memberU0
  , memberR0
  , memberM0
  , member1
  , member2
  , memberU2
  , memberM2
  , member3
  , memberM3
  , member4
  , equalityAndHash
  , comparison
  , negation
  , abs
  , numberPlus
  , numberMinus
  , numberTimes
  , numberDivideBy
  , numberDivideByNonzeroU
  , plus
  , minus
  , times
  , divideBy
  , divideByNonzeroU
  , divideByNonzeroR
  , divideByNonzeroM
  , divMod
  , dot
  , cross
  , nested
  , functions
  )
where

import Data.Hashable (Hashable)
import Data.Hashable qualified
import OpenSolid.API.AbsFunction (AbsFunction (AbsFunction))
import OpenSolid.API.AbsFunction qualified as AbsFunction
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.ComparisonFunction (ComparisonFunction (ComparisonFunction))
import OpenSolid.API.ComparisonFunction qualified as ComparisonFunction
import OpenSolid.API.Constant (Constant (Constant))
import OpenSolid.API.Constant qualified as Constant
import OpenSolid.API.Constructor (Constructor (..))
import OpenSolid.API.Constructor qualified as Constructor
import OpenSolid.API.EqualityFunction (EqualityFunction (EqualityFunction))
import OpenSolid.API.EqualityFunction qualified as EqualityFunction
import OpenSolid.API.Function (Function (..))
import OpenSolid.API.HashFunction (HashFunction (HashFunction))
import OpenSolid.API.HashFunction qualified as HashFunction
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.NegationFunction (NegationFunction (NegationFunction))
import OpenSolid.API.NegationFunction qualified as NegationFunction
import OpenSolid.API.PostOperatorOverload (PostOperatorOverload (..))
import OpenSolid.API.PostOperatorOverload qualified as PostOperatorOverload
import OpenSolid.API.PreOperatorOverload (PreOperatorOverload (..))
import OpenSolid.API.PreOperatorOverload qualified as PreOperatorOverload
import OpenSolid.API.Property (Property (Property))
import OpenSolid.API.Property qualified as Property
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.Angle qualified as Angle
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.HasZero (HasZero)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (cross, dot)
import OpenSolid.Prelude qualified
import OpenSolid.Tolerance qualified as Tolerance

data Class where
  Class ::
    { name :: FFI.ClassName
    , documentation :: Text
    , constants :: List (FFI.Name, Constant)
    , constructor :: Maybe Constructor
    , staticFunctions :: List (FFI.Name, StaticFunction)
    , properties :: List (FFI.Name, Property)
    , memberFunctions :: List (FFI.Name, MemberFunction)
    , equalityAndHashFunctions :: Maybe (EqualityFunction, HashFunction)
    , comparisonFunction :: Maybe ComparisonFunction
    , negationFunction :: Maybe NegationFunction
    , absFunction :: Maybe AbsFunction
    , preOperators :: List (BinaryOperator.Id, NonEmpty PreOperatorOverload)
    , postOperators :: List (BinaryOperator.Id, NonEmpty PostOperatorOverload)
    , nestedClasses :: List Class
    } ->
    Class

data Member value where
  Const :: FFI.Name -> Constant -> Member value
  Constructor :: Constructor -> Member value
  Static :: FFI.Name -> StaticFunction -> Member value
  Prop :: FFI.Name -> Property -> Member value
  Member :: FFI.Name -> MemberFunction -> Member value
  EqualityAndHash :: (EqualityFunction, HashFunction) -> Member value
  Comparison :: ComparisonFunction -> Member value
  Negate :: NegationFunction -> Member value
  Abs :: AbsFunction -> Member value
  DivMod :: FFI (Quantity units) => Member (Quantity units)
  PreOverload :: BinaryOperator.Id -> PreOperatorOverload -> Member value
  PostOverload :: BinaryOperator.Id -> PostOperatorOverload -> Member value
  Nested :: FFI nested => Text -> List (Member nested) -> Member value

new :: forall t. FFI t => Text -> List (Member t) -> Class
new givenDocumentation members =
  buildClass members (init (FFI.className t) givenDocumentation)

static :: Text -> Text -> List (Member Void) -> Class
static className givenDocumentation members =
  buildClass members (init (FFI.staticClassName className) givenDocumentation)

constant :: FFI result => Text -> result -> Text -> Member value
constant name value docs = Const (FFI.name name) (Constant value docs)

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

factoryM1 ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> value) ->
  Text ->
  Member value
factoryM1 = staticM1

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

factory2R ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> Result x value) ->
  Text ->
  Member value
factory2R = static2

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

factory5 ::
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e -> value) ->
  Text ->
  Member value
factory5 = static5

factory6 ::
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e -> f -> value) ->
  Text ->
  Member value
factory6 = static6

static1 :: (FFI a, FFI result) => Text -> Text -> (a -> result) -> Text -> Member value
static1 name arg1 f docs = Static (FFI.name name) (StaticFunction1 (FFI.name arg1) f docs)

staticU1 ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Unitless => a -> result) ->
  Text ->
  Member value
staticU1 name arg1 f docs =
  Static (FFI.name name) $
    StaticFunction1 (FFI.name arg1) (Tolerance.using Tolerance.unitless f) docs

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
  Static (FFI.name name) $
    StaticFunction2 (FFI.name arg1) (FFI.name arg2) (Tolerance.using Tolerance.unitless f) docs

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
  Static (FFI.name name) $
    StaticFunction3
      (FFI.name arg1)
      (FFI.name arg2)
      (FFI.name arg3)
      (Tolerance.using Tolerance.unitless f)
      docs

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
    StaticFunction4
      (FFI.name arg1)
      (FFI.name arg2)
      (FFI.name arg3)
      (FFI.name arg4)
      (Tolerance.using Tolerance.unitless f)
      docs

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

static5 ::
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e -> result) ->
  Text ->
  Member value
static5 name arg1 arg2 arg3 arg4 arg5 f docs =
  Static (FFI.name name) $
    StaticFunction5
      (FFI.name arg1)
      (FFI.name arg2)
      (FFI.name arg3)
      (FFI.name arg4)
      (FFI.name arg5)
      f
      docs

static6 ::
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e -> f -> result) ->
  Text ->
  Member value
static6 name arg1 arg2 arg3 arg4 arg5 arg6 f docs =
  Static (FFI.name name) $
    StaticFunction6
      (FFI.name arg1)
      (FFI.name arg2)
      (FFI.name arg3)
      (FFI.name arg4)
      (FFI.name arg5)
      (FFI.name arg6)
      f
      docs

static10 ::
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h, FFI i, FFI j, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> result) ->
  Text ->
  Member value
static10 name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 f docs =
  Static (FFI.name name) $
    StaticFunction10
      (FFI.name arg1)
      (FFI.name arg2)
      (FFI.name arg3)
      (FFI.name arg4)
      (FFI.name arg5)
      (FFI.name arg6)
      (FFI.name arg7)
      (FFI.name arg8)
      (FFI.name arg9)
      (FFI.name arg10)
      f
      docs

property :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
property name f docs = Prop (FFI.name name) (Property f docs)

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
member0 name f docs = Member (FFI.name name) (MemberFunction0 f docs)

memberU0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Unitless => value -> result) ->
  Text ->
  Member value
memberU0 name f docs =
  Member (FFI.name name) (MemberFunction0 (Tolerance.using Tolerance.unitless f) docs)

memberR0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Radians => value -> result) ->
  Text ->
  Member value
memberR0 name f docs =
  Member (FFI.name name) (MemberFunction0 (Tolerance.using Angle.tolerance f) docs)

memberM0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> result) ->
  Text ->
  Member value
memberM0 name f docs = Member (FFI.name name) (MemberFunctionM0 f docs)

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
  Member (FFI.name name) $
    MemberFunction2
      (FFI.name arg1)
      (FFI.name arg2)
      (Tolerance.using Tolerance.unitless f)
      docs

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

member3 ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value -> result) ->
  Text ->
  Member value
member3 name arg1 arg2 arg3 f docs =
  Member (FFI.name name) (MemberFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

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

member4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> value -> result) ->
  Text ->
  Member value
member4 name arg1 arg2 arg3 arg4 f docs =
  Member (FFI.name name) $
    MemberFunction4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs

equalityAndHash :: forall value. (FFI value, Eq value, Hashable value) => Member value
equalityAndHash =
  EqualityAndHash (EqualityFunction ((==) @value), HashFunction (Data.Hashable.hash @value))

comparison :: forall value. (FFI value, Ord value) => Member value
comparison = Comparison (ComparisonFunction (comparisonImpl @value))

comparisonImpl :: Ord a => a -> a -> Int
comparisonImpl lhs rhs = case compare lhs rhs of LT -> -1; EQ -> 0; GT -> 1

negation :: forall value. (FFI value, Negation value) => Member value
negation = Negate (NegationFunction (negate @value))

abs :: FFI value => (value -> value) -> Member value
abs = Abs . AbsFunction

numberPlus ::
  forall value result.
  (Addition Number value result, FFI value, FFI result) =>
  Member value
numberPlus =
  PreOverload BinaryOperator.Add $
    PreOperatorOverload ((+) :: Number -> value -> result)

numberMinus ::
  forall value result.
  (Subtraction Number value result, FFI value, FFI result) =>
  Member value
numberMinus =
  PreOverload BinaryOperator.Sub $
    PreOperatorOverload ((-) :: Number -> value -> result)

numberTimes ::
  forall value result.
  (Multiplication Number value result, FFI value, FFI result) =>
  Member value
numberTimes =
  PreOverload BinaryOperator.Mul $
    PreOperatorOverload ((*) :: Number -> value -> result)

numberDivideBy ::
  forall value result.
  (Division Number value result, FFI value, FFI result) =>
  Member value
numberDivideBy =
  PreOverload BinaryOperator.Div $
    PreOperatorOverload ((/) :: Number -> value -> result)

numberDivideByNonzeroU ::
  forall value result.
  (Division Number (Nonzero value) result, FFI value, FFI result) =>
  (Tolerance Unitless => value -> Result HasZero (Nonzero value)) ->
  Member value
numberDivideByNonzeroU nonzero = do
  let implementation :: Number -> value -> Result HasZero result
      implementation number value = do
        nonzeroValue <- Tolerance.using Tolerance.unitless (nonzero value)
        Ok (number / nonzeroValue)
  PreOverload BinaryOperator.Div (PreOperatorOverload implementation)

plus ::
  forall rhs value result.
  (Addition value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
plus = do
  let operator :: value -> rhs -> result = (+)
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Add overload

minus ::
  forall rhs value result.
  (Subtraction value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
minus = do
  let operator :: value -> rhs -> result = (-)
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Sub overload

times ::
  forall rhs value result.
  (Multiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
times = do
  let operator :: value -> rhs -> result = (*)
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Mul overload

divideBy ::
  forall rhs value result.
  (Division value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
divideBy = do
  let operator :: value -> rhs -> result = (/)
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Div overload

divideByNonzeroU ::
  forall rhs value result.
  (Division value (Nonzero rhs) result, FFI value, FFI rhs, FFI result) =>
  (Tolerance Unitless => rhs -> Result HasZero (Nonzero rhs)) ->
  Member value
divideByNonzeroU nonzero = do
  let implementation :: value -> rhs -> Result HasZero result
      implementation value rhs = do
        nonzeroRhs <- Tolerance.using Tolerance.unitless (nonzero rhs)
        Ok (value / nonzeroRhs)
  PostOverload BinaryOperator.Div (PostOperatorOverload implementation)

divideByNonzeroR ::
  forall rhs value result.
  (Division value (Nonzero rhs) result, FFI value, FFI rhs, FFI result) =>
  (Tolerance Radians => rhs -> Result HasZero (Nonzero rhs)) ->
  Member value
divideByNonzeroR nonzero = do
  let implementation :: value -> rhs -> Result HasZero result
      implementation value rhs = do
        nonzeroRhs <- Tolerance.using Angle.tolerance (nonzero rhs)
        Ok (value / nonzeroRhs)
  PostOverload BinaryOperator.Div (PostOperatorOverload implementation)

divideByNonzeroM ::
  forall rhs value result.
  (Division value (Nonzero rhs) result, FFI value, FFI rhs, FFI result) =>
  (Tolerance Meters => rhs -> Result HasZero (Nonzero rhs)) ->
  Member value
divideByNonzeroM nonzero = do
  let implementation :: Tolerance Meters => value -> rhs -> Result HasZero result
      implementation value rhs = do
        nonzeroRhs <- nonzero rhs
        Ok (value / nonzeroRhs)
  PostOverload BinaryOperator.Div (PostOperatorOverloadM implementation)

divMod :: FFI (Quantity units) => Member (Quantity units)
divMod = DivMod

dot ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
dot = do
  let operator :: value -> rhs -> result = OpenSolid.Prelude.dot
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Dot overload

cross ::
  forall rhs value result.
  (CrossMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
cross = do
  let operator :: value -> rhs -> result = OpenSolid.Prelude.cross
  let overload = PostOperatorOverload operator
  PostOverload BinaryOperator.Cross overload

nested :: FFI nestedValue => Text -> List (Member nestedValue) -> Member value
nested = Nested

addPreOverload ::
  BinaryOperator.Id ->
  PreOperatorOverload ->
  List (BinaryOperator.Id, NonEmpty PreOperatorOverload) ->
  List (BinaryOperator.Id, NonEmpty PreOperatorOverload)
addPreOverload operatorId overload [] = [(operatorId, NonEmpty.one overload)]
addPreOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, NonEmpty.extend existingOverloads [overload]) : rest
    else first : addPreOverload operatorId overload rest

addPostOverload ::
  BinaryOperator.Id ->
  PostOperatorOverload ->
  List (BinaryOperator.Id, NonEmpty PostOperatorOverload) ->
  List (BinaryOperator.Id, NonEmpty PostOperatorOverload)
addPostOverload operatorId overload [] = [(operatorId, NonEmpty.one overload)]
addPostOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, NonEmpty.extend existingOverloads [overload]) : rest
    else first : addPostOverload operatorId overload rest

init :: FFI.ClassName -> Text -> Class
init givenName givenDocumentation =
  Class
    { name = givenName
    , documentation = givenDocumentation
    , constants = []
    , constructor = Nothing
    , staticFunctions = []
    , properties = []
    , memberFunctions = []
    , equalityAndHashFunctions = Nothing
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
    Const name value ->
      built{constants = built.constants <> [(name, value)]}
    Constructor constructor ->
      built{constructor = Just constructor}
    Static name staticFunction ->
      built{staticFunctions = built.staticFunctions <> [(name, staticFunction)]}
    Prop name prop ->
      built{properties = built.properties <> [(name, prop)]}
    Member name memberFunction ->
      built{memberFunctions = built.memberFunctions <> [(name, memberFunction)]}
    EqualityAndHash functionPair ->
      built{equalityAndHashFunctions = Just functionPair}
    Comparison comparisonFunction ->
      built{comparisonFunction = Just comparisonFunction}
    Negate negationFunction ->
      built{negationFunction = Just negationFunction}
    Abs absFunction ->
      built{absFunction = Just absFunction}
    DivMod @units -> do
      let divOperator :: Quantity units -> Quantity units -> Int = (//)
      let modOperator :: Quantity units -> Quantity units -> Quantity units = (%)
      built
        { postOperators =
            built.postOperators
              & addPostOverload BinaryOperator.FloorDiv (PostOperatorOverload divOperator)
              & addPostOverload BinaryOperator.Mod (PostOperatorOverload modOperator)
        }
    PreOverload operatorId overload ->
      built{preOperators = addPreOverload operatorId overload built.preOperators}
    PostOverload operatorId overload ->
      built{postOperators = addPostOverload operatorId overload built.postOperators}
    Nested nestedDocstring nestedMembers ->
      built{nestedClasses = built.nestedClasses <> [new nestedDocstring nestedMembers]}

----- FUNCTION COLLECTION -----

functions :: Class -> List Function
functions
  ( Class
      className
      _
      constants
      constructor
      staticFunctions
      properties
      memberFunctions
      equalityAndHashFunctions
      comparisonFunction
      negationFunction
      absFunction
      preOperators
      postOperators
      nestedClasses
    ) = do
    List.concat
      [ List.map (constantFunctionInfo className) constants
      , constructorInfo className constructor
      , List.map (staticFunctionInfo className) staticFunctions
      , List.map (propertyInfo className) properties
      , List.map (memberFunctionInfo className) memberFunctions
      , equalityAndHashFunctionInfo className equalityAndHashFunctions
      , comparisonFunctionInfo className comparisonFunction
      , negationFunctionInfo className negationFunction
      , absFunctionInfo className absFunction
      , List.combine (NonEmpty.toList . preOperatorOverloads className) preOperators
      , List.combine (NonEmpty.toList . postOperatorOverloads className) postOperators
      , List.combine functions nestedClasses
      ]

constantFunctionInfo :: FFI.ClassName -> (FFI.Name, Constant) -> Function
constantFunctionInfo className (constantName, constantFunction@(Constant @t _ _)) =
  Function
    { ffiName = Constant.ffiName className constantName
    , implicitTolerance = Nothing
    , argumentTypes = []
    , returnType = FFI.typeOf t
    , invoke = Constant.invoke constantFunction
    }

constructorInfo :: FFI.ClassName -> Maybe Constructor -> List Function
constructorInfo className maybeConstructor = case maybeConstructor of
  Nothing -> []
  Just constructor -> do
    let arguments = Constructor.signature constructor
    List.singleton $
      Function
        { ffiName = Constructor.ffiName className constructor
        , implicitTolerance = Nothing
        , argumentTypes = List.map Pair.second arguments
        , returnType = FFI.Class className
        , invoke = Constructor.invoke constructor
        }

staticFunctionInfo :: FFI.ClassName -> (FFI.Name, StaticFunction) -> Function
staticFunctionInfo className (functionName, staticFunction) = do
  let (implicitTolerance, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = StaticFunction.ffiName className functionName staticFunction
    , implicitTolerance
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

propertyInfo :: FFI.ClassName -> (FFI.Name, Property) -> Function
propertyInfo className (propertyName, prop) = do
  let selfType = FFI.Class className
  Function
    { ffiName = Property.ffiName className propertyName
    , implicitTolerance = Nothing
    , argumentTypes = [selfType]
    , returnType = Property.returnType prop
    , invoke = Property.invoke prop
    }

memberFunctionInfo :: FFI.ClassName -> (FFI.Name, MemberFunction) -> Function
memberFunctionInfo className (functionName, memberFunction) = do
  let selfType = FFI.Class className
  let (implicitTolerance, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = MemberFunction.ffiName className functionName memberFunction
    , implicitTolerance
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
        , implicitTolerance = Nothing
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
        , implicitTolerance = Nothing
        , argumentTypes = [selfType]
        , returnType = selfType
        , invoke = AbsFunction.invoke absFunction
        }

equalityAndHashFunctionInfo ::
  FFI.ClassName ->
  Maybe (EqualityFunction, HashFunction) ->
  List Function
equalityAndHashFunctionInfo className maybeFunctions = case maybeFunctions of
  Nothing -> []
  Just (equalityFunction, hashFunction) -> do
    let selfType = FFI.Class className
    let equalityFunctionInfo =
          Function
            { ffiName = EqualityFunction.ffiName className
            , implicitTolerance = Nothing
            , argumentTypes = [selfType, selfType]
            , returnType = FFI.typeOf Bool
            , invoke = EqualityFunction.invoke equalityFunction
            }
    let hashFunctionInfo =
          Function
            { ffiName = HashFunction.ffiName className
            , implicitTolerance = Nothing
            , argumentTypes = [selfType]
            , returnType = FFI.typeOf Int
            , invoke = HashFunction.invoke hashFunction
            }
    [equalityFunctionInfo, hashFunctionInfo]

comparisonFunctionInfo :: FFI.ClassName -> Maybe ComparisonFunction -> List Function
comparisonFunctionInfo className maybeComparisonFunction = case maybeComparisonFunction of
  Nothing -> []
  Just comparisonFunction -> do
    let selfType = FFI.Class className
    List.singleton $
      Function
        { ffiName = ComparisonFunction.ffiName className
        , implicitTolerance = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf Int
        , invoke = ComparisonFunction.invoke comparisonFunction
        }

preOperatorOverload :: FFI.ClassName -> BinaryOperator.Id -> PreOperatorOverload -> Function
preOperatorOverload className operatorId overload = do
  let selfType = FFI.Class className
  let (implicitTolerance, lhsType, returnType) = PreOperatorOverload.signature overload
  Function
    { ffiName = PreOperatorOverload.ffiName className operatorId overload
    , implicitTolerance = implicitTolerance
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperatorOverload.invoke overload
    }

preOperatorOverloads ::
  FFI.ClassName ->
  (BinaryOperator.Id, NonEmpty PreOperatorOverload) ->
  NonEmpty Function
preOperatorOverloads className (operatorId, overloads) =
  NonEmpty.map (preOperatorOverload className operatorId) overloads

postOperatorOverload ::
  FFI.ClassName ->
  BinaryOperator.Id ->
  PostOperatorOverload ->
  Function
postOperatorOverload className operatorId overload = do
  let selfType = FFI.Class className
  let (implicitTolerance, rhsType, returnType) = PostOperatorOverload.signature overload
  Function
    { ffiName = PostOperatorOverload.ffiName className operatorId overload
    , implicitTolerance = implicitTolerance
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperatorOverload.invoke overload
    }

postOperatorOverloads ::
  FFI.ClassName ->
  (BinaryOperator.Id, NonEmpty PostOperatorOverload) ->
  NonEmpty Function
postOperatorOverloads className (operatorId, overloads) =
  NonEmpty.map (postOperatorOverload className operatorId) overloads
