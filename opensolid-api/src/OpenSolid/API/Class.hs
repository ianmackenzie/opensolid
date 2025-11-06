-- Needed for 'property', since call sites will explicitly specify the property name
{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenSolid.API.Class
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
  , factoryM1
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
  , factory5
  , factory6
  , static1
  , static2
  , static3
  , staticM3
  , property
  , member0
  , memberU0
  , memberR0
  , memberM0
  , memberS0
  , member1
  , member2
  , memberU2
  , memberM2
  , member3
  , memberM3
  , member4
  , equalityAndHash
  , comparison
  , negateSelf
  , absSelf
  , numberPlus
  , numberMinus
  , numberTimes
  , numberDivBy
  , numberDivByU
  , numberDivByR
  , numberDivByM
  , numberDivByS
  , plus
  , plusNumber
  , plusSelf
  , minus
  , minusNumber
  , minusSelf
  , times
  , timesNumber
  , timesSelf
  , divBy
  , divByU
  , divByR
  , divByM
  , divByS
  , divByNumber
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

import Data.Hashable (Hashable)
import Data.Hashable qualified
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
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
import OpenSolid.API.Upcast (Upcast (Upcast))
import OpenSolid.API.Upcast qualified as Upcast
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data Class where
  Class ::
    { name :: FFI.ClassName
    , documentation :: Text
    , toParent :: Maybe Upcast
    , constants :: List (FFI.Name, Constant)
    , constructor :: Maybe Constructor
    , staticFunctions :: List (FFI.Name, StaticFunction)
    , properties :: List (FFI.Name, Property)
    , memberFunctions :: List (FFI.Name, MemberFunction)
    , equalityAndHashFunctions :: Maybe (EqualityFunction, HashFunction)
    , comparisonFunction :: Maybe ComparisonFunction
    , negationFunction :: Maybe NegationFunction
    , absFunction :: Maybe AbsFunction
    , preOperators :: List (BinaryOperator.Id, List PreOperatorOverload)
    , postOperators :: List (BinaryOperator.Id, List PostOperatorOverload)
    , nestedClasses :: List Class
    } ->
    Class

data Member value where
  ToParent :: Upcast -> Member value
  Const :: FFI.Name -> Constant -> Member value
  Constructor :: Constructor -> Member value
  Static :: FFI.Name -> StaticFunction -> Member value
  Prop :: FFI.Name -> Property -> Member value
  Member :: FFI.Name -> MemberFunction -> Member value
  EqualityAndHash :: (EqualityFunction, HashFunction) -> Member value
  Comparison :: ComparisonFunction -> Member value
  Negate :: NegationFunction -> Member value
  Abs :: AbsFunction -> Member value
  PreOverload :: BinaryOperator.Id -> PreOperatorOverload -> Member value
  PostOverload :: BinaryOperator.Id -> PostOperatorOverload -> Member value
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

data Self a = Self

equalityAndHash :: forall value. (FFI value, Eq value, Hashable value) => Member value
equalityAndHash =
  EqualityAndHash (EqualityFunction ((==) @value), HashFunction (Data.Hashable.hash @value))

comparison :: forall value. (FFI value, Ord value) => Member value
comparison = Comparison (ComparisonFunction (comparisonImpl @value))

comparisonImpl :: Ord a => a -> a -> Int
comparisonImpl lhs rhs = case compare lhs rhs of LT -> -1; EQ -> 0; GT -> 1

negateSelf :: forall value. (FFI value, Negation value) => Member value
negateSelf = Negate (NegationFunction (negative @value))

absSelf :: FFI value => (value -> value) -> Member value
absSelf = Abs . AbsFunction

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

numberDivBy ::
  forall value result.
  (Division Number value result, FFI value, FFI result) =>
  Member value
numberDivBy =
  PreOverload BinaryOperator.Div $
    PreOperatorOverload ((/) :: Number -> value -> result)

numberDivByU ::
  (FFI value, FFI result) =>
  (Tolerance Unitless => Number -> value -> result) ->
  Member value
numberDivByU f = PreOverload BinaryOperator.Div (PreOperatorOverloadU f)

numberDivByR ::
  (FFI value, FFI result) =>
  (Tolerance Radians => Number -> value -> result) ->
  Member value
numberDivByR f = PreOverload BinaryOperator.Div (PreOperatorOverloadR f)

numberDivByM ::
  (FFI value, FFI result) =>
  (Tolerance Meters => Number -> value -> result) ->
  Member value
numberDivByM f = PreOverload BinaryOperator.Div (PreOperatorOverloadM f)

numberDivByS ::
  (FFI value, FFI result) =>
  (Tolerance SquareMeters => Number -> value -> result) ->
  Member value
numberDivByS f = PreOverload BinaryOperator.Div (PreOperatorOverloadS f)

plus ::
  forall rhs value result.
  (Addition value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
plus _ =
  PostOverload BinaryOperator.Add $
    PostOperatorOverload ((+) :: value -> rhs -> result)

plusNumber ::
  forall value result.
  (Addition value Number result, FFI value, FFI result) =>
  Member value
plusNumber = plus @Number Self

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
minus _ =
  PostOverload BinaryOperator.Sub $
    PostOperatorOverload ((-) :: value -> rhs -> result)

minusNumber ::
  forall value result.
  (Subtraction value Number result, FFI value, FFI result) =>
  Member value
minusNumber = minus @Number Self

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
times _ =
  PostOverload BinaryOperator.Mul $
    PostOperatorOverload ((*) :: value -> rhs -> result)

timesNumber ::
  forall value result.
  (Multiplication value Number result, FFI value, FFI result) =>
  Member value
timesNumber = times @Number Self

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
divBy _ =
  PostOverload BinaryOperator.Div $
    PostOperatorOverload ((/) :: value -> rhs -> result)

divByU ::
  (FFI value, FFI rhs, FFI result) =>
  (Tolerance Unitless => value -> rhs -> result) ->
  Member value
divByU f = PostOverload BinaryOperator.Div (PostOperatorOverloadU f)

divByR ::
  (FFI value, FFI rhs, FFI result) =>
  (Tolerance Radians => value -> rhs -> result) ->
  Member value
divByR f = PostOverload BinaryOperator.Div (PostOperatorOverloadR f)

divByM ::
  (FFI value, FFI rhs, FFI result) =>
  (Tolerance Meters => value -> rhs -> result) ->
  Member value
divByM f = PostOverload BinaryOperator.Div (PostOperatorOverloadM f)

divByS ::
  (FFI value, FFI rhs, FFI result) =>
  (Tolerance SquareMeters => value -> rhs -> result) ->
  Member value
divByS f = PostOverload BinaryOperator.Div (PostOperatorOverloadS f)

divByNumber ::
  forall value result.
  (Division value Number result, FFI value, FFI result) =>
  Member value
divByNumber = divBy @Number Self

divBySelf ::
  forall value result.
  (Division value value result, FFI value, FFI result) =>
  Member value
divBySelf = divBy @value Self

floorDivBySelf :: forall units. FFI (Quantity units) => Member (Quantity units)
floorDivBySelf =
  PostOverload BinaryOperator.FloorDiv $
    PostOperatorOverload ((//) :: Quantity units -> Quantity units -> Int)

modBySelf :: forall units. FFI (Quantity units) => Member (Quantity units)
modBySelf =
  PostOverload BinaryOperator.Mod $
    PostOperatorOverload ((%) :: Quantity units -> Quantity units -> Quantity units)

dotProduct ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Self (value -> rhs -> result) ->
  Member value
dotProduct _ =
  PostOverload BinaryOperator.Dot $
    PostOperatorOverload (dot :: value -> rhs -> result)

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
crossProduct _ =
  PostOverload BinaryOperator.Cross $
    PostOperatorOverload (cross :: value -> rhs -> result)

crossSelf ::
  forall value result.
  (CrossMultiplication value value result, FFI value, FFI result) =>
  Member value
crossSelf = crossProduct @value Self

nested :: FFI nested => Text -> List (Member nested) -> Member value
nested = Nested

addPreOverload ::
  BinaryOperator.Id ->
  PreOperatorOverload ->
  List (BinaryOperator.Id, List PreOperatorOverload) ->
  List (BinaryOperator.Id, List PreOperatorOverload)
addPreOverload operatorId overload [] = [(operatorId, [overload])]
addPreOverload operatorId overload (first : rest) = do
  let (existingId, existingOverloads) = first
  if operatorId == existingId
    then (existingId, existingOverloads <> [overload]) : rest
    else first : addPreOverload operatorId overload rest

addPostOverload ::
  BinaryOperator.Id ->
  PostOperatorOverload ->
  List (BinaryOperator.Id, List PostOperatorOverload) ->
  List (BinaryOperator.Id, List PostOperatorOverload)
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
    ToParent toParent ->
      built{toParent = Just toParent}
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
    PreOverload operatorId overload ->
      built{preOperators = built.preOperators |> addPreOverload operatorId overload}
    PostOverload operatorId overload ->
      built{postOperators = built.postOperators |> addPostOverload operatorId overload}
    Nested nestedDocstring nestedMembers ->
      built{nestedClasses = built.nestedClasses <> [new nestedDocstring nestedMembers]}

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
      properties
      memberFunctions
      equalityAndHashFunctions
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
      , List.map (propertyInfo className) properties
      , List.map (memberFunctionInfo className) memberFunctions
      , equalityAndHashFunctionInfo className equalityAndHashFunctions
      , comparisonFunctionInfo className comparisonFunction
      , negationFunctionInfo className negationFunction
      , absFunctionInfo className absFunction
      , List.combine (preOperatorOverloads className) preOperators
      , List.combine (postOperatorOverloads className) postOperators
      , List.combine functions nestedClasses
      ]

upcastInfo :: FFI.ClassName -> Maybe Upcast -> List Function
upcastInfo className maybeToParent = case maybeToParent of
  Nothing -> []
  Just toParent -> do
    List.singleton $
      Function
        { ffiName = Upcast.ffiName className
        , implicitArgument = Nothing
        , argumentTypes = [FFI.Class className]
        , returnType = FFI.Class (Upcast.parentClassName toParent)
        , invoke = Upcast.invoke toParent
        }

constantFunctionInfo :: FFI.ClassName -> (FFI.Name, Constant) -> Function
constantFunctionInfo className (constantName, constantFunction@(Constant value _)) =
  Function
    { ffiName = Constant.ffiName className constantName
    , implicitArgument = Nothing
    , argumentTypes = []
    , returnType = Constant.valueType value
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
        , implicitArgument = Nothing
        , argumentTypes = List.map Pair.second arguments
        , returnType = FFI.Class className
        , invoke = Constructor.invoke constructor
        }

staticFunctionInfo :: FFI.ClassName -> (FFI.Name, StaticFunction) -> Function
staticFunctionInfo className (functionName, staticFunction) = do
  let (implicitArgument, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = StaticFunction.ffiName className functionName staticFunction
    , implicitArgument
    , argumentTypes = List.map Pair.second arguments
    , returnType
    , invoke = StaticFunction.invoke staticFunction
    }

propertyInfo :: FFI.ClassName -> (FFI.Name, Property) -> Function
propertyInfo className (propertyName, prop) = do
  let selfType = FFI.Class className
  Function
    { ffiName = Property.ffiName className propertyName
    , implicitArgument = Nothing
    , argumentTypes = [selfType]
    , returnType = Property.returnType prop
    , invoke = Property.invoke prop
    }

memberFunctionInfo :: FFI.ClassName -> (FFI.Name, MemberFunction) -> Function
memberFunctionInfo className (functionName, memberFunction) = do
  let selfType = FFI.Class className
  let (implicitArgument, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let arguments = positionalArguments <> namedArguments
  Function
    { ffiName = MemberFunction.ffiName className functionName memberFunction
    , implicitArgument
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
        , implicitArgument = Nothing
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
        , implicitArgument = Nothing
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
            , implicitArgument = Nothing
            , argumentTypes = [selfType, selfType]
            , returnType = FFI.typeOf @Bool Proxy
            , invoke = EqualityFunction.invoke equalityFunction
            }
    let hashFunctionInfo =
          Function
            { ffiName = HashFunction.ffiName className
            , implicitArgument = Nothing
            , argumentTypes = [selfType]
            , returnType = FFI.typeOf @Int Proxy
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
        , implicitArgument = Nothing
        , argumentTypes = [selfType, selfType]
        , returnType = FFI.typeOf @Int Proxy
        , invoke = ComparisonFunction.invoke comparisonFunction
        }

preOperatorOverload :: FFI.ClassName -> BinaryOperator.Id -> PreOperatorOverload -> Function
preOperatorOverload className operatorId overload = do
  let selfType = FFI.Class className
  let (implicitArgument, lhsType, returnType) = PreOperatorOverload.signature overload
  Function
    { ffiName = PreOperatorOverload.ffiName className operatorId overload
    , implicitArgument = implicitArgument
    , argumentTypes = [lhsType, selfType]
    , returnType = returnType
    , invoke = PreOperatorOverload.invoke overload
    }

preOperatorOverloads ::
  FFI.ClassName ->
  (BinaryOperator.Id, List PreOperatorOverload) ->
  List Function
preOperatorOverloads className (operatorId, overloads) =
  List.map (preOperatorOverload className operatorId) overloads

postOperatorOverload :: FFI.ClassName -> BinaryOperator.Id -> PostOperatorOverload -> Function
postOperatorOverload className operatorId overload = do
  let selfType = FFI.Class className
  let (implicitArgument, rhsType, returnType) = PostOperatorOverload.signature overload
  Function
    { ffiName = PostOperatorOverload.ffiName className operatorId overload
    , implicitArgument = implicitArgument
    , argumentTypes = [selfType, rhsType]
    , returnType = returnType
    , invoke = PostOperatorOverload.invoke overload
    }

postOperatorOverloads ::
  FFI.ClassName ->
  (BinaryOperator.Id, List PostOperatorOverload) ->
  List Function
postOperatorOverloads className (operatorId, overloads) =
  List.map (postOperatorOverload className operatorId) overloads
