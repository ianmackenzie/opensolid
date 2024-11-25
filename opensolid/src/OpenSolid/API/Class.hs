module OpenSolid.API.Class
  ( Class (..)
  , abstract
  , concrete
  )
where

import OpenSolid
import OpenSolid.API.Class.MemberFunction (MemberFunction)
import OpenSolid.API.Class.StaticFunction (StaticFunction)
import OpenSolid.FFI (FFI)

data Class where
  Class ::
    FFI value =>
    { name :: Text
    , staticFunctions :: List (Text, List StaticFunction)
    , memberFunctions :: List (Text, List (MemberFunction value))
    } ->
    Class

abstract :: Text -> List (Text, List StaticFunction) -> Class
abstract name staticFunctions =
  Class
    { name
    , staticFunctions
    , memberFunctions = [] :: List (Text, List (MemberFunction Int))
    }

concrete :: FFI value => Text -> List (Text, List (MemberFunction value)) -> Class
concrete name memberFunctions =
  Class
    { name
    , staticFunctions = []
    , memberFunctions
    }
