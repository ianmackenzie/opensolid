module Api (Api (..), Class (..), Function (..), FunctionKind (..), ValueType (..), ExceptionClass (..)) where

import Maybe (Maybe)
import OpenSolid (Eq, Int, List, Show)
import Text (Text)

newtype Api = Api (List Class)

data Class = Class Text (List Text) (List ExceptionClass) (List Function)

data ExceptionClass
  = ExceptionClass
      Text -- name
      (List (Int, Text, Maybe ValueType)) -- constructors (tag, name, optional argument)
  deriving (Show)

data Function
  = Function
      FunctionKind
      Text -- ffi name
      Text -- human readable name
      (List (Text, ValueType)) -- arguments
      ValueType -- return type

data FunctionKind = Method | Static

data ValueType
  = Pointer Text
  | Float
  | Boolean
  | Maybe ValueType
  | Result Text Text ValueType -- Module name, Exception type, Successful type
  | Tuple2 ValueType ValueType
  | ImplicitTolerance
  | Self
  deriving (Eq, Show)
