module Api (Api (..), Class (..), Function (..), FunctionKind (..), ValueType (..)) where

import Data.String (String)
import OpenSolid (Eq, List)

newtype Api = Api (List Class)

data Class = Class String (List String) (List Function)

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
  | Result String ValueType
  | Tuple2 ValueType ValueType
  | ImplicitTolerance
  deriving (Eq)
