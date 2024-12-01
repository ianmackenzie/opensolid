module OpenSolid.API.Class (Class (..)) where

import OpenSolid
import OpenSolid.API.BinaryOperator (BinaryOperator)
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.MemberFunction (MemberFunction)
import OpenSolid.API.Name (Name)
import OpenSolid.API.StaticFunction (StaticFunction)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI

data Class where
  Class ::
    FFI value =>
    { id :: FFI.Id value
    , staticFunctions :: List (Name, List StaticFunction)
    , memberFunctions :: List (Name, List (MemberFunction value))
    , negationFunction :: Maybe (value -> value)
    , binaryOperators :: List (BinaryOperator.Id, List (BinaryOperator value))
    , nestedClasses :: List Class
    } ->
    Class
