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
  , factoryT1
  , factory1R
  , factoryT1R
  , factory2
  , factory2R
  , factoryT2
  , factoryT2R
  , factory3
  , factoryT3
  , factoryT3R
  , factory4
  , factoryT4
  , factoryT4R
  , factory5
  , factory6
  , static1
  , static1R
  , static1I
  , static2
  , static3
  , staticT3
  , static10
  , property
  , member0
  , member0R
  , member0I
  , memberT0
  , memberT0R
  , memberT0I
  , member1
  , member1I
  , member2
  , member2I
  , memberT2
  , memberT2R
  , memberT2I
  , member3
  , member3I
  , memberT3
  , memberT3I
  , member4
  , member4I
  , equalityAndHash
  , comparison
  , negation
  , abs
  , numberPlus
  , numberMinus
  , numberTimes
  , numberDivideBy
  , numberDivideByNonzero
  , plus
  , minus
  , times
  , divideBy
  , divideByNonzero
  , divideByNonzeroT
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
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.HasZero (HasZero)
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (cross, dot)
import OpenSolid.Prelude qualified
import OpenSolid.Result qualified as Result

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

wrap1 :: (a -> result) -> (a -> IO result)
wrap1 function a = IO.succeed (function a)

wrap2 :: (a -> b -> result) -> (a -> b -> IO result)
wrap2 function a b = IO.succeed (function a b)

wrap3 :: (a -> b -> c -> result) -> (a -> b -> c -> IO result)
wrap3 function a b c = IO.succeed (function a b c)

wrap4 :: (a -> b -> c -> d -> result) -> (a -> b -> c -> d -> IO result)
wrap4 function a b c d = IO.succeed (function a b c d)

wrap5 :: (a -> b -> c -> d -> e -> result) -> (a -> b -> c -> d -> e -> IO result)
wrap5 function a b c d e = IO.succeed (function a b c d e)

wrap6 :: (a -> b -> c -> d -> e -> f -> result) -> (a -> b -> c -> d -> e -> f -> IO result)
wrap6 function a b c d e f = IO.succeed (function a b c d e f)

-- wrap7 ::
--   (a -> b -> c -> d -> e -> f -> g -> result) ->
--   (a -> b -> c -> d -> e -> f -> g -> IO result)
-- wrap7 function a b c d e f g =
--   IO.succeed (function a b c d e f g)
--
-- wrap8 ::
--   (a -> b -> c -> d -> e -> f -> g -> h -> result) ->
--   (a -> b -> c -> d -> e -> f -> g -> h -> IO result)
-- wrap8 function a b c d e f g h =
--   IO.succeed (function a b c d e f g h)
--
-- wrap9 ::
--   (a -> b -> c -> d -> e -> f -> g -> h -> i -> result) ->
--   (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO result)
-- wrap9 function a b c d e f g h i =
--   IO.succeed (function a b c d e f g h i)

wrap10 ::
  (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> result) ->
  (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO result)
wrap10 function a b c d e f g h i j =
  IO.succeed (function a b c d e f g h i j)

wrap1R :: (a -> Result x result) -> a -> IO result
wrap1R function a = function a ?? fail

wrap2R :: (a -> b -> Result x result) -> a -> b -> IO result
wrap2R function a b = function a b ?? fail

wrap3R :: (a -> b -> c -> Result x result) -> a -> b -> c -> IO result
wrap3R function a b c = function a b c ?? fail

wrap4R :: (a -> b -> c -> d -> Result x result) -> a -> b -> c -> d -> IO result
wrap4R function a b c d = function a b c d ?? fail

-- wrap5R :: (a -> b -> c -> d -> e -> Result x result) -> a -> b -> c -> d -> e -> IO result
-- wrap5R function a b c d e = function a b c d e ?? fail

static :: Text -> Text -> List (Member Void) -> Class
static className givenDocumentation members =
  buildClass members (init (FFI.staticClassName className) givenDocumentation)

constant :: FFI result => Text -> result -> Text -> Member value
constant name value docs = Const (FFI.name name) (Constant value docs)

constructor1 :: (FFI a, FFI value) => Text -> (a -> value) -> Text -> Member value
constructor1 arg1 f docs = constructor1I arg1 (wrap1 f) docs

constructor1I :: (FFI a, FFI value) => Text -> (a -> IO value) -> Text -> Member value
constructor1I arg1 f docs = Constructor (Constructor1 (FFI.name arg1) f docs)

constructor2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  (a -> b -> value) ->
  Text ->
  Member value
constructor2 arg1 arg2 f docs = constructor2I arg1 arg2 (wrap2 f) docs

constructor2I ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  (a -> b -> IO value) ->
  Text ->
  Member value
constructor2I arg1 arg2 f docs = Constructor (Constructor2 (FFI.name arg1) (FFI.name arg2) f docs)

constructor3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value) ->
  Text ->
  Member value
constructor3 arg1 arg2 arg3 f docs =
  constructor3I arg1 arg2 arg3 (wrap3 f) docs

constructor3I ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> IO value) ->
  Text ->
  Member value
constructor3I arg1 arg2 arg3 f docs =
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
  constructor4I arg1 arg2 arg3 arg4 (wrap4 f) docs

constructor4I ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> IO value) ->
  Text ->
  Member value
constructor4I arg1 arg2 arg3 arg4 f docs =
  Constructor (Constructor4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) f docs)

factory1 :: (FFI a, FFI value) => Text -> Text -> (a -> value) -> Text -> Member value
factory1 = static1

factoryT1 ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> value) ->
  Text ->
  Member value
factoryT1 = staticT1

factory1R ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (a -> Result x value) ->
  Text ->
  Member value
factory1R = static1R

factoryT1R ::
  (FFI a, FFI value) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> Result x value) ->
  Text ->
  Member value
factoryT1R = staticT1R

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
factory2R = static2R

factoryT2 ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value) ->
  Text ->
  Member value
factoryT2 = staticT2

factoryT2R ::
  (FFI a, FFI b, FFI value) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> Result x value) ->
  Text ->
  Member value
factoryT2R = staticT2R

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

factoryT3 ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> value) ->
  Text ->
  Member value
factoryT3 = staticT3

factoryT3R ::
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> Result x value) ->
  Text ->
  Member value
factoryT3R = staticT3R

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

factoryT4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> value) ->
  Text ->
  Member value
factoryT4 = staticT4

factoryT4R ::
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> Result x value) ->
  Text ->
  Member value
factoryT4R = staticT4R

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

static1 ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (a -> result) ->
  Text ->
  Member value
static1 name arg1 f docs =
  static1I name arg1 (wrap1 f) docs

static1R ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (a -> Result x result) ->
  Text ->
  Member value
static1R name arg1 f docs =
  static1I name arg1 (wrap1R f) docs

static1I ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (a -> IO result) ->
  Text ->
  Member value
static1I name arg1 f docs =
  Static (FFI.name name) (StaticFunction1 (FFI.name arg1) f docs)

staticT1 ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> result) ->
  Text ->
  Member value
staticT1 name arg1 f docs =
  staticT1I name arg1 (wrap1 f) docs

staticT1R ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> Result x result) ->
  Text ->
  Member value
staticT1R name arg1 f docs =
  staticT1I name arg1 (wrap1R f) docs

staticT1I ::
  (FFI a, FFI result) =>
  Text ->
  Text ->
  (Tolerance Meters => a -> IO result) ->
  Text ->
  Member value
staticT1I name arg1 f docs =
  Static (FFI.name name) (StaticFunctionM1 (FFI.name arg1) f docs)

static2 ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> result) ->
  Text ->
  Member value
static2 name arg1 arg2 f docs =
  static2I name arg1 arg2 (wrap2 f) docs

static2R ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> Result x result) ->
  Text ->
  Member value
static2R name arg1 arg2 f docs =
  static2I name arg1 arg2 (wrap2R f) docs

static2I ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> IO result) ->
  Text ->
  Member value
static2I name arg1 arg2 f docs =
  Static (FFI.name name) (StaticFunction2 (FFI.name arg1) (FFI.name arg2) f docs)

staticT2 ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> result) ->
  Text ->
  Member value
staticT2 name arg1 arg2 f docs =
  Static (FFI.name name) (StaticFunctionM2 (FFI.name arg1) (FFI.name arg2) (wrap2 f) docs)

staticT2R ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> Result x result) ->
  Text ->
  Member value
staticT2R name arg1 arg2 f docs =
  staticT2I name arg1 arg2 (wrap2R f) docs

staticT2I ::
  (FFI a, FFI b, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> IO result) ->
  Text ->
  Member value
staticT2I name arg1 arg2 f docs =
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
  Static (FFI.name name) $
    StaticFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (wrap3 f) docs

staticT3 ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> result) ->
  Text ->
  Member value
staticT3 name arg1 arg2 arg3 f docs =
  staticT3I name arg1 arg2 arg3 (wrap3 f) docs

staticT3R ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> Result x result) ->
  Text ->
  Member value
staticT3R name arg1 arg2 arg3 f docs =
  staticT3I name arg1 arg2 arg3 (wrap3R f) docs

staticT3I ::
  (FFI a, FFI b, FFI c, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> IO result) ->
  Text ->
  Member value
staticT3I name arg1 arg2 arg3 f docs =
  Static (FFI.name name) $
    StaticFunctionM3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs

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
    StaticFunction4 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) (FFI.name arg4) (wrap4 f) docs

staticT4 ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> result) ->
  Text ->
  Member value
staticT4 name arg1 arg2 arg3 arg4 f docs =
  staticT4I name arg1 arg2 arg3 arg4 (wrap4 f) docs

staticT4R ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> Result x result) ->
  Text ->
  Member value
staticT4R name arg1 arg2 arg3 arg4 f docs =
  staticT4I name arg1 arg2 arg3 arg4 (wrap4R f) docs

staticT4I ::
  (FFI a, FFI b, FFI c, FFI d, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> d -> IO result) ->
  Text ->
  Member value
staticT4I name arg1 arg2 arg3 arg4 f docs =
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
      (wrap5 f)
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
      (wrap6 f)
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
      (wrap10 f)
      docs

property :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
property name f docs = Prop (FFI.name name) (Property (wrap1 f) docs)

member0 :: (FFI value, FFI result) => Text -> (value -> result) -> Text -> Member value
member0 name f docs = member0I name (wrap1 f) docs

member0R :: (FFI value, FFI result) => Text -> (value -> Result x result) -> Text -> Member value
member0R name f docs = member0I name (wrap1R f) docs

member0I :: (FFI value, FFI result) => Text -> (value -> IO result) -> Text -> Member value
member0I name f docs = Member (FFI.name name) (MemberFunction0 f docs)

memberT0 ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> result) ->
  Text ->
  Member value
memberT0 name f docs = memberT0I name (wrap1 f) docs

memberT0R ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> Result x result) ->
  Text ->
  Member value
memberT0R name f docs = memberT0I name (wrap1R f) docs

memberT0I ::
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> IO result) ->
  Text ->
  Member value
memberT0I name f docs = Member (FFI.name name) (MemberFunctionT0 f docs)

member1 ::
  (FFI a, FFI value, FFI result) =>
  Text ->
  Text ->
  (a -> value -> result) ->
  Text ->
  Member value
member1 name arg1 f docs = member1I name arg1 (wrap2 f) docs

member1I ::
  (FFI a, FFI value, FFI result) =>
  Text ->
  Text ->
  (a -> value -> IO result) ->
  Text ->
  Member value
member1I name arg1 f docs = Member (FFI.name name) (MemberFunction1 (FFI.name arg1) f docs)

member2 ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> value -> result) ->
  Text ->
  Member value
member2 name arg1 arg2 f docs = member2I name arg1 arg2 (wrap3 f) docs

member2I ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> value -> IO result) ->
  Text ->
  Member value
member2I name arg1 arg2 f docs =
  Member (FFI.name name) (MemberFunction2 (FFI.name arg1) (FFI.name arg2) f docs)

memberT2 ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value -> result) ->
  Text ->
  Member value
memberT2 name arg1 arg2 f docs = memberT2I name arg1 arg2 (wrap3 f) docs

memberT2R ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value -> Result x result) ->
  Text ->
  Member value
memberT2R name arg1 arg2 f docs = memberT2I name arg1 arg2 (wrap3R f) docs

memberT2I ::
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> value -> IO result) ->
  Text ->
  Member value
memberT2I name arg1 arg2 f docs =
  Member (FFI.name name) (MemberFunctionT2 (FFI.name arg1) (FFI.name arg2) f docs)

member3 ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value -> result) ->
  Text ->
  Member value
member3 name arg1 arg2 arg3 f docs = member3I name arg1 arg2 arg3 (wrap4 f) docs

member3I ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value -> IO result) ->
  Text ->
  Member value
member3I name arg1 arg2 arg3 f docs =
  Member (FFI.name name) (MemberFunction3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

memberT3 ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  Text ->
  Member value
memberT3 name arg1 arg2 arg3 f docs = memberT3I name arg1 arg2 arg3 (wrap4 f) docs

memberT3I ::
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (Tolerance Meters => a -> b -> c -> value -> IO result) ->
  Text ->
  Member value
memberT3I name arg1 arg2 arg3 f docs =
  Member (FFI.name name) (MemberFunctionT3 (FFI.name arg1) (FFI.name arg2) (FFI.name arg3) f docs)

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
member4 name arg1 arg2 arg3 arg4 f docs = member4I name arg1 arg2 arg3 arg4 (wrap5 f) docs

member4I ::
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> value -> IO result) ->
  Text ->
  Member value
member4I name arg1 arg2 arg3 arg4 f docs =
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
negation = Negate (NegationFunction (wrap1 (negate @value)))

abs :: FFI value => (value -> value) -> Member value
abs = Abs . AbsFunction . wrap1

numberPlus ::
  forall value result.
  (Addition Number value result, FFI value, FFI result) =>
  Member value
numberPlus =
  PreOverload BinaryOperator.Add $
    PreOperatorOverload (wrap2 ((+) :: Number -> value -> result))

numberMinus ::
  forall value result.
  (Subtraction Number value result, FFI value, FFI result) =>
  Member value
numberMinus =
  PreOverload BinaryOperator.Sub $
    PreOperatorOverload (wrap2 ((-) :: Number -> value -> result))

numberTimes ::
  forall value result.
  (Multiplication Number value result, FFI value, FFI result) =>
  Member value
numberTimes =
  PreOverload BinaryOperator.Mul $
    PreOperatorOverload (wrap2 ((*) :: Number -> value -> result))

numberDivideBy ::
  forall value result.
  (Division Number value result, FFI value, FFI result) =>
  Member value
numberDivideBy =
  PreOverload BinaryOperator.Div $
    PreOperatorOverload (wrap2 ((/) :: Number -> value -> result))

numberDivideByNonzero ::
  forall value result.
  (Division Number (Nonzero value) result, FFI value, FFI result) =>
  (value -> Result HasZero (Nonzero value)) ->
  Member value
numberDivideByNonzero nonzero = do
  let implementation :: Number -> value -> Result HasZero result
      implementation number value = Result.map (number /) (nonzero value)
  PreOverload BinaryOperator.Div (PreOperatorOverload (wrap2R implementation))

plus ::
  forall rhs value result.
  (Addition value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
plus = do
  let operator :: value -> rhs -> result = (+)
  let overload = PostOperatorOverload (wrap2 operator)
  PostOverload BinaryOperator.Add overload

minus ::
  forall rhs value result.
  (Subtraction value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
minus = do
  let operator :: value -> rhs -> result = (-)
  let overload = PostOperatorOverload (wrap2 operator)
  PostOverload BinaryOperator.Sub overload

times ::
  forall rhs value result.
  (Multiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
times = do
  let operator :: value -> rhs -> result = (*)
  let overload = PostOperatorOverload (wrap2 operator)
  PostOverload BinaryOperator.Mul overload

divideBy ::
  forall rhs value result.
  (Division value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
divideBy = do
  let operator :: value -> rhs -> result = (/)
  let overload = PostOperatorOverload (wrap2 operator)
  PostOverload BinaryOperator.Div overload

divideByNonzero ::
  forall rhs value result.
  (Division value (Nonzero rhs) result, FFI value, FFI rhs, FFI result) =>
  (rhs -> Result HasZero (Nonzero rhs)) ->
  Member value
divideByNonzero nonzero = do
  let implementation :: value -> rhs -> Result HasZero result
      implementation value rhs = Result.map (value /) (nonzero rhs)
  PostOverload BinaryOperator.Div (PostOperatorOverload (wrap2R implementation))

divideByNonzeroT ::
  forall rhs value result.
  (Division value (Nonzero rhs) result, FFI value, FFI rhs, FFI result) =>
  (Tolerance Meters => rhs -> Result HasZero (Nonzero rhs)) ->
  Member value
divideByNonzeroT nonzero = do
  let implementation :: Tolerance Meters => value -> rhs -> Result HasZero result
      implementation value rhs = Result.map (value /) (nonzero rhs)
  PostOverload BinaryOperator.Div (PostOperatorOverloadT (wrap2R implementation))

divMod :: FFI (Quantity units) => Member (Quantity units)
divMod = DivMod

dot ::
  forall rhs value result.
  (DotMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
dot = do
  let operator :: value -> rhs -> result = OpenSolid.Prelude.dot
  let overload = PostOperatorOverload (wrap2 operator)
  PostOverload BinaryOperator.Dot overload

cross ::
  forall rhs value result.
  (CrossMultiplication value rhs result, FFI value, FFI rhs, FFI result) =>
  Member value
cross = do
  let operator :: value -> rhs -> result = OpenSolid.Prelude.cross
  let overload = PostOperatorOverload (wrap2 operator)
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
              & addPostOverload BinaryOperator.FloorDiv (PostOperatorOverload (wrap2 divOperator))
              & addPostOverload BinaryOperator.Mod (PostOperatorOverload (wrap2 modOperator))
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
