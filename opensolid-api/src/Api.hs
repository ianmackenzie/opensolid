module Api (Api (..), Class (..), Function (..), FunctionKind (..), Type (..)) where

import Data.String (String)

newtype Api = Api [Class]

data Class = Class String [String] [Function]

data Function
  = Function
      FunctionKind
      String -- ffi name
      String -- human readable name
      [(String, Type)] -- list of arguments
      Type -- return type

data FunctionKind = Method | Static

data Type = Pointer String | Float
