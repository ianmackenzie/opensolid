module Api (Api (..), Class (..), Function (..), FunctionKind (..), ValueType (..)) where

import Data.String (String)

newtype Api = Api [Class]

data Class = Class String [String] [Function]

data Function
  = Function
      FunctionKind
      String -- ffi name
      String -- human readable name
      [(String, ValueType)] -- list of arguments
      ValueType -- return type

data FunctionKind = Method | Static

data ValueType = Pointer String | Float
