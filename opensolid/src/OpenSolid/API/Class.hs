module OpenSolid.API.Class
  ( Class (..)
  , abstract
  , concrete
  )
where

import OpenSolid
import OpenSolid.API.Class.MemberFunction (MemberFunction)
import OpenSolid.API.Class.StaticFunction (StaticFunction)
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)

data Class where
  Class ::
    FFI value =>
    { name :: Name
    , units :: Maybe Name
    , staticFunctions :: List (Text, List StaticFunction)
    , memberFunctions :: List (Text, List (MemberFunction value))
    } ->
    Class

abstract :: Text -> List (Text, List StaticFunction) -> Class
abstract name staticFunctions =
  Class
    { name = Name.parse name
    , units = Nothing
    , staticFunctions
    , memberFunctions = [] :: List (Text, List (MemberFunction Int))
    }

concrete :: FFI value => Text -> Text -> List (Text, List (MemberFunction value)) -> Class
concrete name units memberFunctions =
  Class
    { name = Name.parse name
    , units = Just (Name.parse units)
    , staticFunctions = []
    , memberFunctions
    }
