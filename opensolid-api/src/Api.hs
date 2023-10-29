module Api (Api (..), Class (..), Function (..), FunctionKind (..), ValueType (..), ExceptionClass (..)) where

import OpenSolid

newtype Api = Api (List Class)

data Class = Class String (List String) (List ExceptionClass) (List Function)

data ExceptionClass
  = ExceptionClass
      String -- name
      (List (Int, String, Maybe ValueType)) -- constructors (tag, name, optional argument)
  deriving (Show)

data Function
  = Function
      FunctionKind
      String -- ffi name
      String -- human readable name
      (List (String, ValueType)) -- arguments
      ValueType -- return type

data FunctionKind = Method | Static

data ValueType
  = Pointer String
  | Float
  | Boolean
  | Maybe ValueType
  | Result String String ValueType -- Module name, Exception type, Successful type
  | Tuple2 ValueType ValueType
  | ImplicitTolerance
  | Self
  deriving (Eq, Show)
