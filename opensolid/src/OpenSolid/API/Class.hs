module OpenSolid.API.Class
  ( Class (..)
  , outer
  )
where

import OpenSolid
import OpenSolid.API.Class.Constructor (Constructor)
import OpenSolid.API.Class.MemberFunction (MemberFunction)
import OpenSolid.API.Class.StaticFunction (StaticFunction)
import OpenSolid.FFI (FFI)

data Class where
  Class ::
    FFI value =>
    { name :: Text
    , constructors :: List (Constructor value)
    , staticFunctions :: List (Text, List StaticFunction)
    , memberFunctions :: List (Text, List (MemberFunction value))
    , nestedClasses :: List Class
    } ->
    Class

outer :: Text -> List (Text, List StaticFunction) -> List Class -> Class
outer name staticFunctions nestedClasses =
  Class
    { name
    , constructors = [] :: List (Constructor Int)
    , staticFunctions
    , memberFunctions = [] :: List (Text, List (MemberFunction Int))
    , nestedClasses
    }
